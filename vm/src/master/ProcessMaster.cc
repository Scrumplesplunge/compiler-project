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
                bind(&ProcessMaster::onRequestInstance, &master_, id_, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessMaster::onInstanceExited, &master_, id_, _1));

  messenger_.ON(CHANNEL_INPUT,
                bind(&ChannelMaster::onInput, &master_.channels_, id_, _1));
  messenger_.ON(CHANNEL_OUTPUT,
                bind(&ChannelMaster::onOutput, &master_.channels_, id_, _1));
  messenger_.ON(CHANNEL_ENABLE,
                bind(&ChannelMaster::onEnable, &master_.channels_, id_, _1));
  messenger_.ON(CHANNEL_DISABLE,
                bind(&ChannelMaster::onDisable, &master_.channels_, id_, _1));
  messenger_.ON(CHANNEL_RESOLVED,
                bind(&ChannelMaster::onResolved, &master_.channels_, id_, _1));
  messenger_.ON(CHANNEL_DONE,
                bind(&ChannelMaster::onDone, &master_.channels_, id_, _1));

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
    instance_id id, InstanceDescriptor descriptor, Ancestry ancestry,
    int32_t initialization_value) {
  MESSAGE(START_INSTANCE) message;
  message.ancestry = ancestry;
  message.descriptor = descriptor;
  message.id = id;
  message.initialization_value = initialization_value;
  send(message);
}

void ProcessServerHandle::instanceStarted(
    instance_id id, instance_id parent_id, int32_t handle_address) {
  MESSAGE(INSTANCE_STARTED) message;
  message.id = id;
  message.parent_id = parent_id;
  message.handle_address = handle_address;
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
      metadata_(loadMetaData(config.metadata_file)),
      channels_(*this) {
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
  instance_id id;
  {
    unique_lock<mutex> process_lock(process_mu_);
    id = process_tree_.createRootInstance(
        InstanceInfo(root_worker, descriptor));
    workers_[root_worker]->startInstance(
        id, descriptor, process_tree_.link(id, root_worker), 0);
  }

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

void ProcessMaster::onRequestInstance(
    worker_id worker, MESSAGE(REQUEST_INSTANCE)&& message) {
  unique_lock<mutex> process_lock(process_mu_);

  // TODO: Decide more sensibly about where to start the next process.
  worker_id new_worker = (next_worker_to_use_++) % workers_.size();

  // Generate an instance ID.
  InstanceInfo info(new_worker, message.descriptor);
  info.parent_id = message.parent_id;
  instance_id id = process_tree_.createInstance(info);

  // Send the start message.
  verr << "Spawning instance " << id << " with parent " << message.parent_id
       << " on worker " << new_worker << "..\n";
  workers_[new_worker]->startInstance(
      id, message.descriptor, process_tree_.link(id, new_worker),
      message.initialization_value);
  
  // Send the instance ID to the parent.
  workers_[worker]->instanceStarted(
      id, message.parent_id, message.handle_address);
}

void ProcessMaster::onInstanceExited(
    worker_id worker, MESSAGE(INSTANCE_EXITED)&& message) {
  unique_lock<mutex> process_lock(process_mu_);
  verr << "Instance " << message.id << " exited.\n";

  // Look up the parent instance ID.
  InstanceInfo info = process_tree_.info(message.id);
  
  // If the instance had a parent, inform the corresponding worker.
  if (info.id == info.parent_id) {
    verr << "Instance was a root.\n";
  } else {
    InstanceInfo parent_info = process_tree_.info(info.parent_id);
    verr << "Informing parent at " << parent_info.location << "..\n";;
    workers_[parent_info.location]->send(move(message));
  }

  process_tree_.endInstance(message.id);
}
