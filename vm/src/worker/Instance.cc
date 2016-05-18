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
  write(workspace_pointer - 12, Ready);
  schedule(workspace_descriptor, queue_lock);
  on_wake_.notify_all();
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

  // Save the handle address into Wptr[0].
  write(Wptr, C);

  // Deschedule this process.
  {
    unique_lock<mutex> lock(queue_mu_);
    waiting_processes_++;
    write(Wptr - 4, Iptr);
  }

  // Send the request.
  server_.requestInstance(descriptor, id_, makeWdesc(Wptr));

  unique_lock<mutex> lock(queue_mu_);
  resumeNext(lock);
}

void Instance::joinInstance() {
  // Deschedule this process.
  {
    unique_lock<mutex> lock(queue_mu_);
    waiting_processes_++;
    write(Wptr - 4, Iptr);
  }

  // Wait for the process to finish.
  server_.joinInstance(A, id_, makeWdesc(Wptr));

  unique_lock<mutex> lock(queue_mu_);
  resumeNext(lock);
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

  Channel channel = server_.process_tree_.channel(id_, C);
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
