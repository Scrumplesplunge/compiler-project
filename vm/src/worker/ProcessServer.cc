#include "ProcessServer.h"

#include "Instance.h"

#include <functional>
#include <string.h>
#include <thread>
#include <util/args.h>

using namespace std;
using namespace std::placeholders;

ProcessServer::ProcessServer(Socket&& socket)
    : messenger_(move(socket)) {
  if (options::verbose)
    cerr << "Configuring messenger channel..\n";
  messenger_.ON(START_PROCESS_SERVER,
                bind(&ProcessServer::onStartProcessServer, this, _1));
  messenger_.ON(START_INSTANCE,
                bind(&ProcessServer::onStartInstance, this, _1));
  messenger_.ON(INSTANCE_STARTED,
                bind(&ProcessServer::onInstanceStarted, this, _1));
  messenger_.ON(INSTANCE_EXITED,
                bind(&ProcessServer::onInstanceExited, this, _1));
}

void ProcessServer::serve() {
  try {
    messenger_.serve();
  } catch (...) {
    if (options::verbose)
      cerr << "Messenger closed. Shutting down.\n";
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
  // Send the exit notification.
  MESSAGE(INSTANCE_EXITED) exit_message;
  exit_message.id = id;
  send(exit_message);

  // Clean up the instance.
  unique_lock<mutex> lock(instance_mu_);
  instances_.erase(id);
}

void ProcessServer::joinInstance(
    instance_id join_id, instance_id waiter_id, int32_t workspace_descriptor) {
  unique_lock<mutex> lock(exit_mu_);
  if (unjoined_exits_.count(join_id) > 0) {
    if (options::verbose)
      cerr << "Instance " << join_id << " has already terminated.\n";
    // Instance has already exited.
    unjoined_exits_.erase(join_id);

    unique_lock<mutex> lock(instance_mu_);
    Instance& instance = *instances_.at(waiter_id);
    instance.wake(workspace_descriptor);
  } else {
    if (options::verbose)
      cerr << "Waiting for instance " << join_id << " to terminate..\n";
    // Instance has not exited yet.
    on_exited_[join_id] = WaitingProcess{waiter_id, workspace_descriptor};
  }
}

void ProcessServer::onStartProcessServer(
    MESSAGE(START_PROCESS_SERVER)&& message) {
  if (options::verbose) {
    cerr << "==========\n"
         << "Job Name       : " << message.name << "\n"
         << "Description    : " << message.description << "\n"
         << "Data Blob Size : " << message.data.length() << "\n"
         << "Bytecode Size  : " << message.bytecode.length() << "\n"
         << "==========\n";
  }

  // Convert the data blob into an int32 array.
  string data(move(message.data));
  data_end_ = VM::MostNeg + data.length();
  data_.reset(new int32_t[(data.length() + 3) / 4]);
  VM::encodeStatic(data, data_.get());
  
  bytecode_ = move(message.bytecode);
  is_ready_ = true;
}

void ProcessServer::onStartInstance(
    MESSAGE(START_INSTANCE)&& message) {
  if (options::verbose) {
    cerr << ::toString(message.type) << " received.\n"
         << "Instance ID : " << message.id << "\n"
         << "Workspace   : " << message.descriptor.workspace_pointer << "\n"
         << "Instruction : " << message.descriptor.instruction_pointer << "\n"
         << "Size        : " << message.descriptor.bytes_needed << "\n";
  }

  unique_lock<mutex> lock(instance_mu_);
  instances_.emplace(message.id, make_unique<Instance>(
      *this, message.id, message.descriptor, bytecode_.c_str(),
      bytecode_.length(), data_.get(), data_end_));

  Instance* instance = instances_.at(message.id).get();

  thread([this, instance] {
    // Create the VM instance.
    if (options::verbose)
      cerr << "Constructing instance..\n";

    // Run it.
    if (options::verbose)
      cerr << "Running..\n";
    instance->run();
    notifyExited(instance->id());
  }).detach();
}

void ProcessServer::onInstanceStarted(MESSAGE(INSTANCE_STARTED)&& message) {
  unique_lock<mutex> lock(instance_mu_);

  // Write back the instance handle and wake up the process.
  Instance& instance = *instances_.at(message.parent_id);
  int32_t workspace_pointer =
      Instance::makeWptr(message.parent_workspace_descriptor);
  instance.write(instance.read(workspace_pointer), message.id);
  instance.wake(message.parent_workspace_descriptor);
}

void ProcessServer::onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message) {
  unique_lock<mutex> lock(exit_mu_);
  if (on_exited_.count(message.id) > 0) {
    if (options::verbose)
      cerr << "Instance " << message.id << " has terminated. Waking waiter.\n";
    // A process is waiting. Reschedule it.
    WaitingProcess& process = on_exited_.at(message.id);

    unique_lock<mutex> lock(instance_mu_);
    instances_.at(process.id)->wake(process.workspace_descriptor);

    on_exited_.erase(message.id);
  } else {
    if (options::verbose)
      cerr << "Instance " << message.id << " has terminated, no waiter.\n";
    // No process is waiting. Remember that this one has exited.
    unjoined_exits_.insert(message.id);
  }
}
