#include "ProcessServer.h"

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

  messenger_.ON(PING, bind(&ProcessServer::onPing, this, _1));
}

void ProcessServer::serve() {
  try {
    messenger_.serve();
  } catch (...) {
    verr << "Messenger closed. Shutting down.\n";
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
  instance_threads_.at(id).detach();
  instance_threads_.erase(id);
}

void ProcessServer::joinInstance(
    instance_id join_id, instance_id waiter_id, int32_t workspace_descriptor) {
  unique_lock<mutex> lock(exit_mu_);
  if (unjoined_exits_.count(join_id) > 0) {
    verr << "Instance " << join_id << " has already terminated.\n";
    // Instance has already exited.
    unjoined_exits_.erase(join_id);

    unique_lock<mutex> lock(instance_mu_);
    Instance& instance = *instances_.at(waiter_id);
    instance.wake(workspace_descriptor);
  } else {
    verr << "Waiting for instance " << join_id << " to terminate..\n";
    // Instance has not exited yet.
    on_exited_[join_id] = WaitingProcess{waiter_id, workspace_descriptor};
  }
}

void ProcessServer::onStartProcessServer(
    MESSAGE(START_PROCESS_SERVER)&& message) {
  verr << "INCOMING: " << ::toString(message.type) << "\n"
       << "Job Name       : " << message.name << "\n"
       << "Description    : " << message.description << "\n"
       << "Data Blob Size : " << message.data.length() << "\n"
       << "Bytecode Size  : " << message.bytecode.length() << "\n";

  // Convert the data blob into an int32 array.
  string data(move(message.data));
  data_end_ = VM::MostNeg + data.length();
  data_.reset(new int32_t[(data.length() + 3) / 4]);
  VM::encodeStatic(data, data_.get());
  
  bytecode_ = move(message.bytecode);
  is_ready_ = true;
}

void ProcessServer::onStartInstance(MESSAGE(START_INSTANCE)&& message) {
  verr << "INCOMING: " << ::toString(message.type) << "\n"
       << "Instance ID : " << message.id << "\n"
       << "Workspace   : " << message.descriptor.workspace_pointer << "\n"
       << "Instruction : " << message.descriptor.instruction_pointer << "\n"
       << "Size        : " << message.descriptor.bytes_needed << "\n";

  unique_lock<mutex> lock(instance_mu_);
  instances_.emplace(message.id, make_unique<Instance>(
      *this, message.id, message.descriptor, bytecode_.c_str(),
      bytecode_.length(), data_.get(), data_end_));

  Instance* instance = instances_.at(message.id).get();

  instance_threads_.emplace(message.id, thread([this, instance] {
    // Create the VM instance.
    verr << "Constructing instance..\n";

    // Run it.
    verr << "Running..\n";
    instance->run();
    notifyExited(instance->id());
  }));
}

void ProcessServer::onInstanceStarted(MESSAGE(INSTANCE_STARTED)&& message) {
  verr << "INCOMING: " << ::toString(message.type) << "\n";

  unique_lock<mutex> lock(instance_mu_);

  // Write back the instance handle and wake up the process.
  Instance& instance = *instances_.at(message.parent_id);
  int32_t workspace_pointer =
      Instance::makeWptr(message.parent_workspace_descriptor);
  instance.write(instance.read(workspace_pointer), message.id);
  instance.wake(message.parent_workspace_descriptor);
}

void ProcessServer::onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message) {
  verr << "INCOMING: " << ::toString(message.type) << "\n";

  unique_lock<mutex> lock(exit_mu_);
  if (on_exited_.count(message.id) > 0) {
    verr << "Instance " << message.id << " has terminated. Waking waiter.\n";
    // A process is waiting. Reschedule it.
    WaitingProcess& process = on_exited_.at(message.id);

    unique_lock<mutex> lock(instance_mu_);
    instances_.at(process.id)->wake(process.workspace_descriptor);

    on_exited_.erase(message.id);
  } else {
    verr << "Instance " << message.id << " has terminated, no waiter.\n";
    // No process is waiting. Remember that this one has exited.
    unjoined_exits_.insert(message.id);
  }
}

void ProcessServer::onPing(MESSAGE(PING)&& message) {
  verr << "INCOMING: " << ::toString(message.type) << "\n";

  send(MESSAGE(PONG)());
}
