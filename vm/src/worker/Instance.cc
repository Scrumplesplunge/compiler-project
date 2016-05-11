#include "Instance.h"

#include "ProcessServer.h"

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
  while (FptrReg == NotProcess && waiting_processes_ > 0) on_wake_.wait(lock);

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
  descriptor.instruction_pointer = C;
  descriptor.bytes_needed = 4 * B;

  // Save the handle address into Wptr[0].
  write(Wptr, A);

  // Deschedule this process.
  {
    unique_lock<mutex> lock(queue_mu_);
    waiting_processes_++;
  }

  // Send the request.
  server_.requestInstance(descriptor, id_, makeWdesc(Wptr));

  {
    unique_lock<mutex> lock(queue_mu_);
    deschedule(lock);
  }
}

void Instance::joinInstance() {
  // Deschedule this process.
  {
    unique_lock<mutex> lock(queue_mu_);
    waiting_processes_++;
  }

  // Wait for the process to finish.
  server_.joinInstance(A, id_, makeWdesc(Wptr));

  {
    unique_lock<mutex> lock(queue_mu_);
    deschedule(lock);
  }
}
