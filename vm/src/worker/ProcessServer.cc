#include "ProcessServer.h"

#include "../util.h"
#include "Instance.h"

#include <functional>
#include <string.h>
#include <util/atomic_output.h>

using namespace std;
using namespace std::placeholders;

ProcessServer::ProcessServer(Socket&& socket)
    : messenger_(move(socket)) {
  verr << "Configuring messenger channel..\n";
  messenger_.ON(START_PROCESS_SERVER,
                bind(&ProcessServer::onStartProcessServer, this, _1));
  messenger_.ON(START_INSTANCE,
                bind(&ProcessServer::onStartInstance, this, _1));
  messenger_.ON(INSTANCE_STARTED,
                bind(&ProcessServer::onInstanceStarted, this, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessServer::onInstanceExited, this, _1));

  messenger_.ON(CHANNEL_IN,
                bind(&ProcessServer::onChannelInput, this, _1));
  messenger_.ON(CHANNEL_OUT,
                bind(&ProcessServer::onChannelOutput, this, _1));
  messenger_.ON(CHANNEL_OUT_DONE,
                bind(&ProcessServer::onChannelOutputDone, this, _1));
  messenger_.ON(CHANNEL_ENABLE,
                bind(&ProcessServer::onChannelEnable, this, _1));
  messenger_.ON(CHANNEL_DISABLE,
                bind(&ProcessServer::onChannelDisable, this, _1));
  messenger_.ON(CHANNEL_RESET,
                bind(&ProcessServer::onChannelReset, this, _1));

  messenger_.ON(PING, bind(&ProcessServer::onPing, this, _1));
}

void ProcessServer::serve() {
  try {
    verr << "Serving " << messenger_.hostPort() << "\n";
    messenger_.serve();
  } catch (const socket_error& error) {
    verr << "Connection closed. Shutting down.\n";
  } catch (const read_error& error) {
    verr << "Read failed. Shutting down.\n";
  }
}

void ProcessServer::requestInstance(
    InstanceDescriptor descriptor, instance_id parent,
    int32_t workspace_descriptor) {
  // Send the request.
  MESSAGE(REQUEST_INSTANCE) message;
  message.parent_id = parent;
  message.descriptor = descriptor;
  message.parent_workspace_descriptor = workspace_descriptor;
  send(message);
}

void ProcessServer::notifyExited(instance_id id) {
  // Remove the instance from the process tree.
  process_tree_.removeLocalInstance(id);

  // Send the exit notification.
  MESSAGE(INSTANCE_EXITED) exit_message;
  exit_message.id = id;
  send(exit_message);

  // Clean up the instance.
  unique_lock<mutex> lock(instance_mu_);
  instances_.erase(id);
  instance_threads_.at(id).detach();
  instance_threads_.erase(id);
}

void ProcessServer::joinInstance(
    instance_id join_id, instance_id waiter_id, int32_t workspace_descriptor) {
  bool unjoined; 
  {
    unique_lock<mutex> exit_lock(exit_mu_);
    unjoined = (unjoined_exits_.count(join_id) > 0);
    if (unjoined) {
      unjoined_exits_.erase(join_id);
    } else {
      on_exited_.emplace(
          join_id, WaitingProcess{waiter_id, workspace_descriptor});
    }
  }

  if (unjoined) {
    unique_lock<mutex> instance_lock(instance_mu_);
    Instance& instance = *instances_.at(waiter_id);
    instance.wake(workspace_descriptor);
  }
}

void ProcessServer::onStartProcessServer(
    MESSAGE(START_PROCESS_SERVER)&& message) {
  verr << "==========\n"
       << "Job Name       : " << message.name << "\n"
       << "Description    : " << message.description << "\n"
       << "Data Blob Size : " << message.data.length() << "\n"
       << "Bytecode Size  : " << message.bytecode.length() << "\n"
       << "==========\n";

  id_ = message.id;

  // Convert the data blob into an int32 array.
  string data(move(message.data));
  data_size_ = data.length();
  data_.reset(new int32_t[(data_size_ + 3) / 4]);
  VM::encodeStatic(data, data_.get());
  
  bytecode_ = move(message.bytecode);
  is_ready_ = true;
}

void ProcessServer::onStartInstance(MESSAGE(START_INSTANCE)&& message) {
  verr << "Starting instance " << message.id << " with Wptr = "
       << addressString(message.descriptor.workspace_pointer) << ", Iptr = "
       << addressString(message.descriptor.instruction_pointer) << "\n";

  // Add it to the process tree.
  process_tree_.addLocalInstance(
      InstanceInfo(id_, message.descriptor), message.ancestry);

  // Construct the new instance.
  Instance* instance;
  {
    unique_lock<mutex> instance_lock(instance_mu_);
    instances_.emplace(message.id, make_unique<Instance>(
        *this, message.id, message.descriptor, bytecode_.c_str(),
        bytecode_.length(), data_.get(), data_size_));
    instance = instances_.at(message.id).get();
  }

  // Run it.
  instance_threads_.emplace(message.id, thread([this, instance] {
    if (options::debug) {
      verr << "Running in debug mode.\n";
      instance->begin();
      while (instance->running()) {
        instance->step(true);
      }
    } else {
      instance->run();
    }
    notifyExited(instance->id());
  }));
}

void ProcessServer::onInstanceStarted(MESSAGE(INSTANCE_STARTED)&& message) {
  verr << ::toString(message.type) << "(" << message.id << ") received.\n";
  unique_lock<mutex> lock(instance_mu_);

  // Write back the instance handle and wake up the process.
  Instance& instance = *instances_.at(message.parent_id);
  int32_t workspace_pointer =
      Instance::makeWptr(message.parent_workspace_descriptor);
  instance.write(instance.read(workspace_pointer), message.id);
  instance.wake(message.parent_workspace_descriptor);
}

void ProcessServer::onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message) {
  verr << ::toString(message.type) << "(" << message.id << ") received.\n";
  bool waiting;
  WaitingProcess process;
  {
    unique_lock<mutex> lock(exit_mu_);
    waiting = (on_exited_.count(message.id) > 0);
    if (waiting) {
      process = on_exited_.at(message.id);
      on_exited_.erase(message.id);
    } else {
      unjoined_exits_.insert(message.id);
    }
  }

  if (waiting) {
    unique_lock<mutex> lock(instance_mu_);
    instances_.at(process.id)->wake(process.workspace_descriptor);
  }
}

void ProcessServer::resolveChannel(
    Channel channel, WaitingReader reader, WaitingWriter writer) {
  Instance* reader_instance;
  Instance* writer_instance;
  bool reader_local, writer_local;
  {
    unique_lock<mutex> instance_lock(instance_mu_);

    reader_local = (instances_.count(reader.id) > 0);
    writer_local = (instances_.count(writer.id) > 0);
    if (reader_local) reader_instance = instances_.at(reader.id).get();
    if (writer_local) writer_instance = instances_.at(writer.id).get();
  }

  verr << "Reader is " << (reader_local ? "local" : "not local")
       << ", writer is " << (writer_local ? "local" : "not local")
       << ".\n";

  if (reader_local && writer_local) {
    // Both are local. Resolve here.
    reader_instance->channelInputDone(channel, move(writer.data));
    writer_instance->channelOutputDone(channel);
  } else if (reader_local && !writer_local) {
    // Wake up the reader and inform the writer.
    reader_instance->channelInputDone(channel, move(writer.data));
    MESSAGE(CHANNEL_OUT_DONE) response;
    response.actor = reader.id;
    response.writer = writer.id;
    response.channel = channel;
    send(response);
  } else if (!reader_local && writer_local) {
    // Forward the write.
    MESSAGE(CHANNEL_OUT) forward;
    forward.actor = writer.id;
    forward.channel = channel;
    forward.data = move(writer.data);
    send(forward);
  }
}

void ProcessServer::resolveEnabled(
    Channel channel, WaitingReader reader, WaitingWriter writer) {
  Instance* reader_instance;
  bool reader_local, writer_local;
  {
    unique_lock<mutex> instance_lock(instance_mu_);

    reader_local = (instances_.count(reader.id) > 0);
    writer_local = (instances_.count(writer.id) > 0);
    if (reader_local) reader_instance = instances_.at(reader.id).get();
  }

  if (reader_local) {
    // Both are local. Wake the reader.
    reader_instance->altWake(channel);
  } else if (writer_local) {
    // Forward the write.
    MESSAGE(CHANNEL_OUT) forward;
    forward.actor = writer.id;
    forward.channel = channel;
    forward.data = move(writer.data);
    send(forward);
  }
}

void ProcessServer::onChannelInput(MESSAGE(CHANNEL_IN)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  WaitingReader reader;
  reader.id = message.actor;
 
  bool writer_waiting;
  WaitingWriter writer;
  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    writer_waiting = (channels_.writers.count(message.channel) > 0);
    if (writer_waiting) {
      writer = move(channels_.writers.at(message.channel));
      channels_.writers.erase(message.channel);
    } else {
      channels_.readers.emplace(message.channel, reader);
    }
  }

  if (writer_waiting) {
    // Writer has already acted, so the transaction can be resolved
    // immediately.
    verr << "Writer was waiting. Resolving..\n";
    resolveChannel(message.channel, reader, writer);
  } else {
    // Writer has not yet acted. Save the reader in the reader map and propogate
    // it.
    bool reader_local, owner_local;
    {
      unique_lock<mutex> instance_lock(instance_mu_);
      reader_local = (instances_.count(reader.id) > 0);
      owner_local = (instances_.count(message.channel.owner) > 0);
    }
    verr << "Reader is " << (reader_local ? "local" : "not local")
         << ", owner is " << (owner_local ? "local" : "not local")
         << ".\n";
    if (reader_local && !owner_local) {
      // Inform the master.
      verr << "Informing master.\n";
      send(message);
    }
  }
}

void ProcessServer::onChannelOutput(MESSAGE(CHANNEL_OUT)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  WaitingWriter writer;
  writer.id = message.actor;
  writer.data = message.data;

  bool reader_waiting, enabled;
  WaitingReader reader;
  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    reader_waiting = (channels_.readers.count(message.channel) > 0);
    enabled = (channels_.enabled.count(message.channel) > 0);
    if (reader_waiting) {
      reader = move(channels_.readers.at(message.channel));
      channels_.readers.erase(message.channel);
    } else if (enabled) {
      reader = move(channels_.enabled.at(message.channel));
      channels_.enabled.erase(message.channel);
    } else {
      channels_.writers.emplace(message.channel, writer);
    }
  }
  if (reader_waiting) {
    // Reader is already waiting, so the transaction can be resolved
    // immediately.
    verr << "Reader was waiting. Resolving..\n";
    resolveChannel(message.channel, reader, writer);
  } else if (enabled) {
    // Reader is waiting in an ALT.
    verr << "Channel is enabled. Resolving..\n";
    resolveEnabled(message.channel, reader, writer);
  } else {
    bool writer_local, owner_local;
    {
      unique_lock<mutex> instance_lock(instance_mu_);
      writer_local = (instances_.count(writer.id) > 0);
      owner_local = (instances_.count(message.channel.owner) > 0);
    }
    verr << "Writer is " << (writer_local ? "local" : "not local")
         << ", owner is " << (owner_local ? "local" : "not local")
         << ".\n";
    if (writer_local && !owner_local) {
      // Inform the master.
      verr << "Informing master.\n";
      send(message);
    }
  }
}

void ProcessServer::onChannelOutputDone(MESSAGE(CHANNEL_OUT_DONE)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    channels_.enabled.erase(message.channel);
    channels_.readers.erase(message.channel);
    channels_.writers.erase(message.channel);
  }

  bool reader_local, writer_local;
  Instance* writer_instance;
  {
    unique_lock<mutex> instance_lock(instance_mu_);
    reader_local = (instances_.count(message.actor) > 0);
    writer_local = (instances_.count(message.writer) > 0);
    if (writer_local) writer_instance = instances_.at(message.writer).get();
  }
  verr << "Reader is " << (reader_local ? "local" : "not local")
       << ", writer is " << (writer_local ? "local" : "not local")
       << ".\n";
  if (writer_local) {
    // Inform the local process.
    writer_instance->channelOutputDone(message.channel);
  } else if (reader_local) {
    // Need to inform a remote process. Propogate the message.
    send(message);
  }
}

void ProcessServer::onChannelEnable(MESSAGE(CHANNEL_ENABLE)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  WaitingReader reader;
  reader.id = message.actor;

  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    channels_.enabled.emplace(message.channel, reader);
  }

  bool reader_local, owner_local;
  {
    unique_lock<mutex> instance_lock(instance_mu_);
    reader_local = (instances_.count(reader.id) > 0);
    owner_local = (instances_.count(message.channel.owner) > 0);
  }
  if (reader_local && !owner_local) {
    // Inform the master.
    send(message);
  }
}

void ProcessServer::onChannelDisable(MESSAGE(CHANNEL_DISABLE)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    channels_.enabled.erase(message.channel);
  }

  bool reader_local, owner_local;
  {
    unique_lock<mutex> instance_lock(instance_mu_);
    reader_local = (instances_.count(message.actor) > 0);
    owner_local = (instances_.count(message.channel.owner) > 0);
  }
  if (reader_local && !owner_local) {
    // Inform the master.
    send(message);
  }
}

void ProcessServer::onChannelReset(MESSAGE(CHANNEL_RESET)&& message) {
  verr << ::toString(message.type) << "("
       << addressString(message.channel.address) << ") received.\n";

  {
    unique_lock<mutex> channel_lock(channels_.mutex);
    channels_.enabled.erase(message.channel);
    channels_.readers.erase(message.channel);
    channels_.writers.erase(message.channel);
  }

  bool actor_local, owner_local;
  {
    unique_lock<mutex> instance_lock(instance_mu_);
    actor_local = (instances_.count(message.actor) > 0);
    owner_local = (instances_.count(message.channel.owner) > 0);
  }
  if (actor_local && !owner_local) {
    // Inform the master.
    send(message);
  }
}

void ProcessServer::onPing(MESSAGE(PING)&& message) {
  send(MESSAGE(PONG)());
}
