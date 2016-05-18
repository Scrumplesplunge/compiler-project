#include "ProcessServer.h"

#include "../util.h"
#include "Instance.h"

#include <functional>
#include <string.h>
#include <util/atomic_output.h>

using namespace std;
using namespace std::placeholders;

ProcessServer::ProcessServer(Socket&& socket)
    : channels_(*this), messenger_(move(socket)) {
  verr << "Configuring messenger channel..\n";
  messenger_.ON(START_PROCESS_SERVER,
                bind(&ProcessServer::onStartProcessServer, this, _1));
  messenger_.ON(START_INSTANCE,
                bind(&ProcessServer::onStartInstance, this, _1));
  messenger_.ON(INSTANCE_STARTED,
                bind(&ProcessServer::onInstanceStarted, this, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessServer::onInstanceExited, this, _1));

  messenger_.ON(PING, bind(&ProcessServer::onPing, this, _1));
}

void ProcessServer::serve() {
  try {
    verr << "Serving " << messenger_.hostPort() << "\n";
    messenger_.serve();
  } catch (const socket_error& error) {
    verr << "Connection closed: " << error.what() << "\n";
  } catch (const read_error& error) {
    verr << "Read failed: " << error.what() << "\n";
  }
}

bool ProcessServer::hasInstance(instance_id id) {
  unique_lock<mutex> instance_lock(instance_mu_);
  return instances_.count(id) > 0;
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
  verr << "Instance " << id << " exited.\n";

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
  verr << ::toString(message.type) << "(" << message.id << ") with Wptr = "
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
      while (instance->running()) instance->step();
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

void ProcessServer::onPing(MESSAGE(PING)&& message) {
  send(MESSAGE(PONG)());
}
