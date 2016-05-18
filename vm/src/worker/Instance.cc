#include "Instance.h"

#include "../util.h"
#include "ProcessServer.h"

#include <util/atomic_output.h>

using namespace std;

Instance::Instance(
    ProcessServer& server, instance_id id, const InstanceDescriptor& descriptor,
    const char* bytecode, int32_t bytecode_size, const int32_t* static_data,
    int32_t static_data_size)
    : VM(descriptor.workspace_pointer - descriptor.bytes_needed + 4,
         descriptor.bytes_needed, static_data, static_data_size, bytecode,
         bytecode_size),
      id_(id), server_(server) {
  set_workspace_pointer(descriptor.workspace_pointer);
  set_instruction_pointer(descriptor.instruction_pointer);
}

void Instance::reschedule(int32_t workspace_descriptor) {
  unique_lock<mutex> queue_lock(queue_mu_);
  waiting_processes_--;
  schedule(workspace_descriptor, queue_lock);
  on_wake_.notify_all();
}

void Instance::altWake(int32_t workspace_descriptor) {
  unique_lock<mutex> queue_lock(queue_mu_);
  waiting_processes_--;
  int32_t workspace_pointer = makeWptr(workspace_descriptor);
  if (read(workspace_pointer - 12) != Ready) {
    write(workspace_pointer - 12, Ready);
    schedule(workspace_descriptor, queue_lock);
    on_wake_.notify_all();
  }
}

void Instance::childStarted(int32_t handle_address, instance_id id) {
  unique_lock<mutex> queue_lock(queue_mu_);
  ChildHandle& handle = children_.at(handle_address);
  switch (handle.state) {
    case ChildHandle::NO_ID:
      // Parent is not waiting.
      verr << "Child ID arrived: " << id << "\n";
      handle.state = ChildHandle::ID;
      handle.id = id;
      break;
    case ChildHandle::JOIN_NO_ID:
      // Parent is waiting, but not on the instance. Now that the ID has
      // arrived, the join can be converted to the correct type.
      verr << "Joining child " << id << "\n";
      queue_lock.unlock();
      server_.joinInstance(id, id_, handle.workspace_descriptor);
      queue_lock.lock();
      children_.erase(handle_address);
      break;
    default:
      throw logic_error("Bad handle state in childStarted().");
  }
}

string Instance::toString() {
  return "Instance = " + to_string(id_) + "\t" + VM::toString();
}

void Instance::runSpecialInstruction(Indirect op) {
  switch (op) {
    case STARTI: startInstance();   break;
    case JOINI:  joinInstance();    break;
    default:
      VM::runSpecialInstruction(op);
  }
}

void Instance::onEmptyProcessQueue(unique_lock<mutex>& lock) {
  // Wait for a process to be rescheduled.
  verr << "Instance " << id_ << " suspended.\n";
  while (FptrReg == NotProcess && waiting_processes_ > 0) on_wake_.wait(lock);
  verr << "Instance " << id_ << " resumed.\n";

  // If no process is has been rescheduled, then none will be.
  if (FptrReg == NotProcess) return VM::onEmptyProcessQueue(lock);

  // Otherwise, resume the new process.
  return resumeNext(lock);
}

void Instance::wake(int32_t workspace_descriptor) {
  unique_lock<mutex> lock(queue_mu_);
  schedule(workspace_descriptor, lock);
  waiting_processes_--;
  on_wake_.notify_all();
}

void Instance::startInstance() {
  // Create the instance descriptor.
  InstanceDescriptor descriptor;
  descriptor.workspace_pointer = Wptr;
  descriptor.instruction_pointer = A;
  descriptor.bytes_needed = 4 * B;

  {
    unique_lock<mutex> queue_lock(queue_mu_);
    children_.emplace(C, ChildHandle());
  }

  // Send the request.
  server_.requestInstance(descriptor, id_, C, read(C));
}

void Instance::joinInstance() {
  // Deschedule this process.
  unique_lock<mutex> queue_lock(queue_mu_);
  waiting_processes_++;
  write(Wptr - 4, Iptr);

  ChildHandle& handle = children_.at(A);

  switch (handle.state) {
    case ChildHandle::NO_ID:
      verr << "Instance " << id_ << " waiting for child ID.\n";
      // No ID is available. Wait for the ID to arrive.
      handle.state = ChildHandle::JOIN_NO_ID;
      handle.workspace_descriptor = makeWdesc(Wptr);
      break;
    case ChildHandle::ID:
      verr << "Instance " << id_ << " waiting for " << handle.id << ".\n";
      // ID is available. Wait for the process to finish.
      queue_lock.unlock();
      server_.joinInstance(handle.id, id_, makeWdesc(Wptr));
      queue_lock.lock();
      children_.erase(A);
      break;
    default:
      throw logic_error("Bad handle state in joinInstance().");
  }

  resumeNext(queue_lock);
}

void Instance::indirect_ALT() {
  VM::indirect_ALT();
}

void Instance::indirect_ALTEND() {
  VM::indirect_ALTEND();
}

void Instance::indirect_ALTWT() {
  unique_lock<mutex> queue_lock(queue_mu_);

  write(Wptr, NoneSelected);  // Indicate that no branch yet selected.

  if (read(Wptr - 12) != Ready) {
    waiting_processes_++;
    deschedule(queue_lock);
  }
}

void Instance::indirect_DISC() {
  // Skip processing if the guard condition is false.
  if (!B) {
    A = False;
    return;
  }

  Channel channel = server_.process_tree_.channel(id_, C);
  if (server_.channels_.disable(channel)) {
    // Guard is ready.
    if (read(Wptr) == NoneSelected) {
      // Channel is the first ready guard.
      write(Wptr, A);
      A = True;
    } else {
      // Another guard was ready first.
      A = False;
    }
  }
}

void Instance::indirect_ENBC() {
  if (!A) return;

  Channel channel = server_.process_tree_.channel(id_, B);
  if (server_.channels_.enable(this, makeWdesc(Wptr), channel)) {
    // Guard is ready.
    write(Wptr - 12, Ready);
  }
}

void Instance::indirect_IN() {
  Channel channel = server_.process_tree_.channel(id_, B);

  {
    unique_lock<mutex> queue_lock(queue_mu_);
    waiting_processes_++;
    write(Wptr - 4, Iptr);
  }

  bool is_ready =
      server_.channels_.input(this, makeWdesc(Wptr), C, A, channel);

  unique_lock<mutex> queue_lock(queue_mu_);
  if (is_ready) {
    waiting_processes_--;
  } else {
    // Channel is not ready. Wait.
    deschedule(queue_lock);
  }
}

void Instance::indirect_OUT() {
  Channel channel = server_.process_tree_.channel(id_, B);

  {
    unique_lock<mutex> queue_lock(queue_mu_);
    waiting_processes_++;
    write(Wptr - 4, Iptr);
  }

  bool is_ready =
      server_.channels_.output(this, makeWdesc(Wptr), C, A, channel);
  
  unique_lock<mutex> queue_lock(queue_mu_);
  if (is_ready) {
    waiting_processes_--;
  } else {
    // Channel is not ready. Wait.
    deschedule(queue_lock);
  }
}
