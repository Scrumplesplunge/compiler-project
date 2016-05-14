#include "ProcessMaster.h"

#include "../util.h"
#include "../runtime/VM.h"

#include <chrono>
#include <fstream>
#include <functional>
#include <iostream>
#include <thread>
#include <util/atomic_output.h>
#include <util/binary.h>
#include <util/stream.h>
#include <vector>

using namespace std;
using namespace std::chrono;
using namespace std::placeholders;

ProcessServerHandle::ProcessServerHandle(
    const string& job_name, const string& job_description,
    ProcessMaster& master, worker_id id, Socket&& socket, string data,
    string bytecode)
    : id_(id), master_(master), messenger_(move(socket)) {
  verr << "Configuring messenger channel..\n";
  messenger_.ON(REQUEST_INSTANCE,
                bind(&ProcessMaster::onRequestInstance, &master_, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessMaster::onInstanceExited, &master_, _1));
  messenger_.ON(CHANNEL_IN,
                bind(&ProcessMaster::onChannelInput, &master_, _1));
  messenger_.ON(CHANNEL_OUT,
                bind(&ProcessMaster::onChannelOutput, &master_, _1));
  messenger_.ON(CHANNEL_OUT_DONE,
                bind(&ProcessMaster::onChannelOutputDone, &master_, _1));
  messenger_.ON(CHANNEL_ENABLE,
                bind(&ProcessMaster::onChannelEnable, &master_, _1));
  messenger_.ON(CHANNEL_DISABLE,
                bind(&ProcessMaster::onChannelDisable, &master_, _1));
  messenger_.ON(CHANNEL_RESET,
                bind(&ProcessMaster::onChannelReset, &master_, _1));

  messenger_.ON(PONG, bind(&ProcessServerHandle::onPong, this, _1));

  // Send the handshake message.
  MESSAGE(START_PROCESS_SERVER) message;
  message.name = job_name;
  message.description = job_description;
  message.data = move(data);
  message.bytecode = move(bytecode);
  send(message);
}

void ProcessServerHandle::startInstance(
    instance_id id, InstanceDescriptor descriptor) {
  MESSAGE(START_INSTANCE) message;
  message.descriptor = descriptor;
  message.id = id;
  send(message);
}

void ProcessServerHandle::instanceStarted(instance_id id, instance_id parent_id,
                                          int32_t parent_workspace_descriptor) {
  MESSAGE(INSTANCE_STARTED) message;
  message.id = id;
  message.parent_id = parent_id;
  message.parent_workspace_descriptor = parent_workspace_descriptor;
  send(message);
}

void ProcessServerHandle::serve() {
  try {
    verr << "Serving " << messenger_.hostPort() << "\n";
    messenger_.serve();
  } catch (const read_error& error) {
    verr << "Connection to " << messenger_.hostPort()
         << " severed. Shutting down.\n";
  }
}

void ProcessServerHandle::close() {
  messenger_.close();
}

int64_t ProcessServerHandle::latency() {
  unique_lock<mutex> lock(latency_mu_);

  // Wait for other pings to finish.
  while (ping_active_) on_latency_complete_.wait(lock);
  ping_active_ = true;
  pong_back_ = false;

  // Perform the ping.
  high_resolution_clock::time_point start = high_resolution_clock::now();
  send(MESSAGE(PING)());
  while (!pong_back_) on_pong_.wait(lock);
  high_resolution_clock::time_point end = high_resolution_clock::now();

  // Wake up waiting calls.
  on_latency_complete_.notify_all();

  return duration_cast<nanoseconds>(end - start).count();
}

void ProcessServerHandle::onPong(MESSAGE(PONG)&& message) {
  unique_lock<mutex> lock(latency_mu_);

  pong_back_ = true;
  on_pong_.notify_all();
}

ProcessMaster::ProcessMaster(const JobConfig& config)
    : job_name_(config.name), job_description_(config.description),
      bytecode_(getFileContents(config.bytecode_file)),
      metadata_(loadMetaData(config.metadata_file)) {
  // Connect to all the workers.
  for (const WorkerAddress& address : config.workers) {
    verr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    socket.connect(address.host, address.port);
    workers_.push_back(make_unique<ProcessServerHandle>(
        job_name_, job_description_, *this, workers_.size(), move(socket),
        metadata_.static_data, bytecode_));
  }
}

void ProcessMaster::serve() {
  // Set up the first instance.
  InstanceDescriptor descriptor;
  descriptor.workspace_pointer = VM::TopWptr;
  descriptor.instruction_pointer = 0;
  descriptor.bytes_needed = metadata_.root_process_size;

  // Start it on the last worker.
  worker_id root_worker = workers_.size() - 1;
  instance_id id = process_tree_.createRootInstance(
      InstanceInfo(root_worker, descriptor));
  workers_[root_worker]->startInstance(id, descriptor);

  verr << "Spawning root process on " << workers_[root_worker]->hostPort()
       << "..\n";

  vector<thread> messenger_tasks;

  // Start serving the worker messages.
  verr << "Starting messenger threads..\n";
  for (auto& worker : workers_) {
    messenger_tasks.push_back(
        thread(&ProcessServerHandle::serve, worker.get()));

    if (options::verbose) {
      int64_t latency = worker->latency();
      verr << "Latency for " << worker->hostPort() << ":\n"
           << formatDuration(latency) << "\n";
    }
  }

  // Wait for the root process to finish.
  verr << "Waiting for root process to terminate..\n";
  process_tree_.join(id);

  // Close each worker connection and wait for the server for that connection to
  // exit.
  verr << "Root process exited. Closing connections..\n";
  for (int i = 0, n = workers_.size(); i < n; i++) {
    workers_[i]->close();
    messenger_tasks[i].join();
  }
}

void ProcessMaster::onRequestInstance(MESSAGE(REQUEST_INSTANCE)&& message) {
  // TODO: Decide more sensibly about where to start the next process.
  worker_id worker = (next_worker_to_use_++) % workers_.size();;

  // Generate an instance ID.
  instance_id id = process_tree_.createInstance(
      InstanceInfo(worker, message.descriptor));

  // Send the start message.
  verr << "Spawning instance " << id << " with parent " << message.parent_id
       << " on worker " << worker << "..\n";
  workers_[worker]->startInstance(id, message.descriptor);
  
  // Send the instance ID to the parent.
  worker_id parent_worker = process_tree_.info(message.parent_id).location;
  workers_[parent_worker]->instanceStarted(
      id, message.parent_id, message.parent_workspace_descriptor);
}

void ProcessMaster::onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message) {
  verr << "Instance " << message.id << " exited.\n";
  // Look up the parent instance ID.
  InstanceInfo info = process_tree_.info(message.id);
  process_tree_.endInstance(message.id);
  
  // If the instance had a parent, inform the corresponding worker.
  if (info.id != info.parent_id) {
    InstanceInfo parent_info = process_tree_.info(info.parent_id);
    workers_[parent_info.location]->send(move(message));
  }
}

void ProcessMaster::onChannelInput(MESSAGE(CHANNEL_IN)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  WaitingReader reader;
  reader.id = message.actor;

  if (channel_writers_.count(message.channel) > 0) {
    // Writer has already acted, so the transaction can be resolved immediately.
    WaitingWriter writer = move(channel_writers_.at(message.channel));
    channel_writers_.erase(message.channel);

    resolveChannelMessage(message.channel, move(reader), move(writer));
  } else {
    // Writer has not yet acted. Save the reader in the reader map.
    channel_readers_.emplace(message.channel, reader);
  }
}

void ProcessMaster::onChannelOutput(MESSAGE(CHANNEL_OUT)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  WaitingWriter writer;
  writer.id = message.actor;
  writer.data = message.data;

  if (channel_readers_.count(message.channel) > 0) {
    // Reader is already waiting, so the transaction can be resolved
    // immediately.
    WaitingReader reader = move(channel_readers_.at(message.channel));
    channel_readers_.erase(message.channel);

    resolveChannelMessage(message.channel, move(reader), move(writer));
  } else if (enabled_channels_.count(message.channel) > 0) {
    // Reader is enabled. Forward the output message.
    WaitingReader reader = enabled_channels_.at(message.channel);
    channel_readers_.erase(message.channel);

    forwardOutput(message.channel, reader, writer);
  } else {
    // Reader is not waiting. Store the output message.
    channel_writers_.emplace(message.channel, writer);
  }
}

void ProcessMaster::onChannelOutputDone(MESSAGE(CHANNEL_OUT_DONE)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  worker_id writer_worker = process_tree_.info(message.actor).location;
  workers_[writer_worker]->send(message);
}

void ProcessMaster::onChannelEnable(MESSAGE(CHANNEL_ENABLE)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  WaitingReader reader;
  reader.id = message.actor;

  if (channel_writers_.count(message.channel) > 0) {
    // Writer has already acted. Forward the output.
    WaitingWriter writer = channel_writers_.at(message.channel);
    forwardOutput(message.channel, reader, writer);
  } else {
    // Writer has not yet acted. Save in the enabled map.
    enabled_channels_.emplace(message.channel, reader);
  }
}

void ProcessMaster::onChannelDisable(MESSAGE(CHANNEL_DISABLE)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  // Writer has not yet acted. Save in the enabled map. Nothing else is
  // necessary: if a write had happened prior to the enable, it would have been
  // passed to the process server for handling at that time. Likewise if one had
  // happened between the enable and now.
  enabled_channels_.erase(message.channel);
}

void ProcessMaster::onChannelReset(MESSAGE(CHANNEL_RESET)&& message) {
  unique_lock<mutex> lock(channel_mu_);

  // Clear any fields related to this channel.
  enabled_channels_.erase(message.channel);
  channel_readers_.erase(message.channel);
  channel_writers_.erase(message.channel);
}

void ProcessMaster::forwardOutput(
    Channel channel, const WaitingReader& reader, const WaitingWriter& writer) {
  worker_id reader_worker = process_tree_.info(reader.id).location;

  // Send the stored data to the reader.
  MESSAGE(CHANNEL_OUT) response;
  response.actor = writer.id;
  response.channel = channel;
  response.data = move(writer.data);

  workers_[reader_worker]->send(response);
}

void ProcessMaster::outputDone(Channel channel, instance_id writer) {
  worker_id writer_worker = process_tree_.info(writer).location;

  // Wake up the writer.
  MESSAGE(CHANNEL_OUT_DONE) done;
  done.channel = channel;
  workers_[writer_worker]->send(done);
}

void ProcessMaster::resolveChannelMessage(
    Channel channel, const WaitingReader& reader, const WaitingWriter& writer) {
  forwardOutput(channel, reader, writer);
  outputDone(channel, writer.id);
}

size_t ProcessMaster::ChannelHasher::operator()(const Channel& channel) const {
  return channel.owner ^ channel.address;
}
