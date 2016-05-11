#include "ProcessMaster.h"

#include "../util.h"
#include "../runtime/VM.h"

#include <fstream>
#include <functional>
#include <iostream>
#include <thread>
#include <util/args.h>
#include <util/binary.h>
#include <util/stream.h>
#include <util/socket.h>
#include <vector>

using namespace std;
using namespace std::placeholders;

ProcessServerHandle::ProcessServerHandle(
    const string& job_name, const string& job_description, ProcessMaster& master,
    worker_id id, Socket&& socket, int32_t data_start, string data,
    string bytecode)
    : id_(id), master_(master), messenger_(move(socket)) {
  if (options::verbose)
    cerr << "Configuring messenger channel..\n";
  messenger_.ON(REQUEST_INSTANCE,
                bind(&ProcessServerHandle::onRequestInstance, this, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessServerHandle::onInstanceExited, this, _1));

  // Send the handshake message.
  MESSAGE(START_PROCESS_SERVER) message;
  message.name = job_name;
  message.description = job_description;
  message.data_start = data_start;
  message.data = move(data);
  message.bytecode = move(bytecode);
  send(message);
}

void ProcessServerHandle::startInstance(
    instance_id id, InstanceDescriptor&& descriptor) {
  MESSAGE(START_INSTANCE) message;
  message.descriptor = move(descriptor);
  message.id = id;
  send(message);
}

void ProcessServerHandle::serve() {
  try {
    messenger_.serve();
  } catch (const read_error& error) {
    if (options::verbose) {
      cerr << "Connection to " << messenger_.hostPort()
           << " severed. Shutting down.\n";
    }
  }
}

void ProcessServerHandle::close() {
  messenger_.close();
}

void ProcessServerHandle::onRequestInstance(
    MESSAGE(REQUEST_INSTANCE)&& message) {
  master_.onRequestInstance(move(message));
}

void ProcessServerHandle::onInstanceExited(
    MESSAGE(INSTANCE_EXITED)&& message) {
  master_.onInstanceExited(move(message));
}

ProcessMaster::ProcessMaster(const JobConfig& config)
    : job_name_(config.name), job_description_(config.description),
      bytecode_(getFileContents(config.bytecode_file)),
      metadata_(loadMetaData(config.metadata_file)) {
  // Connect to all the workers.
  for (const WorkerAddress& address : config.workers) {
    if (options::verbose)
      cerr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    socket.connect(address.host, address.port);
    workers_.push_back(make_unique<ProcessServerHandle>(
        job_name_, job_description_, *this, workers_.size(), move(socket),
        metadata_.memory_start, metadata_.static_data, bytecode_));
  }
}

void ProcessMaster::serve() {
  // Set up the first instance.
  InstanceDescriptor descriptor;
  descriptor.workspace_pointer = metadata_.workspace_pointer;
  descriptor.instruction_pointer = 0;
  descriptor.space_needed = 
      metadata_.memory_start - metadata_.workspace_pointer +
      metadata_.memory_size;

  // Start it on the last worker.
  worker_id root_worker = workers_.size() - 1;
  instance_id id = process_tree_.createRootInstance(root_worker);
  workers_[root_worker]->startInstance(id, move(descriptor));

  if (options::verbose) {
    cerr << "Spawning root process on " << workers_[root_worker]->hostPort()
         << "..\n";
  }

  vector<thread> messenger_tasks;

  // Start serving the worker messages.
  if (options::verbose)
    cerr << "Starting messenger threads..\n";
  for (auto& worker : workers_) {
    messenger_tasks.push_back(
        thread(&ProcessServerHandle::serve, worker.get()));
  }

  // Wait for the root process to finish.
  if (options::verbose)
    cerr << "Waiting for root process to terminate..\n";
  process_tree_.join(id);

  // Close each worker connection and wait for the server for that connection to
  // exit.
  if (options::verbose)
    cerr << "Root process exited. Closing connections..\n";
  for (int i = 0, n = workers_.size(); i < n; i++) {
    workers_[i]->close();
    messenger_tasks[i].join();
  }
}

void ProcessMaster::onRequestInstance(
    MESSAGE(REQUEST_INSTANCE)&& message) {
  startInstance(message.parent_id, move(message.descriptor));
}

void ProcessMaster::onInstanceExited(
    MESSAGE(INSTANCE_EXITED)&& message) {
  // Look up the parent instance ID.
  InstanceInfo info = process_tree_.info(message.id);
  process_tree_.endInstance(message.id);
  
  // If the instance had a parent, inform the corresponding worker.
  if (info.id != info.parent_id)
    workers_[info.location]->send(move(message));
}

void ProcessMaster::startInstance(
    instance_id parent_id, InstanceDescriptor&& descriptor) {
  // TODO: Decide more sensibly about where to start the next process.
  worker_id worker = next_worker_to_use_;
  next_worker_to_use_ = (next_worker_to_use_ + 1) % workers_.size();

  // Generate an instance ID.
  instance_id id = process_tree_.createInstance(parent_id, worker);

  // Send the start message.
  workers_[worker]->startInstance(id, move(descriptor));
}
