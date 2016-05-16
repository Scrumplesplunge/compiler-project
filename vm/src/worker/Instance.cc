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
  verr << "instance[" << id_ << "].wake()\n";
  unique_lock<mutex> lock(queue_mu_);
  schedule(workspace_descriptor, lock);
  waiting_processes_--;
  on_wake_.notify_all();
}

void Instance::altWake(Channel channel) {
  verr << "instance[" << id_ << "].altWake("
       << addressString(channel.address) << ")\n";
  if (alt_sleep) {
    int32_t workspace_descriptor;
    if (id_ == channel.owner) {
      workspace_descriptor = read(channel.address);
    } else {
      unique_lock<mutex> channel_lock(server_.channels_.mutex);
      WaitingProcess process = server_.channels_.processes.at(channel);
      if (process.id != id_)
        throw logic_error("This instance should be the waiting one.");
      workspace_descriptor = process.workspace_descriptor;
    }

    unique_lock<mutex> queue_lock(queue_mu_);
    schedule(workspace_descriptor, queue_lock);
    alt_sleep = false;
    waiting_processes_--;
    on_wake_.notify_all();
  }
}

void Instance::channelInputDone(Channel channel, string&& data) {
  verr << "instance[" << id_ << "].channelInputDone("
       << addressString(channel.address) << ", data[" << data.length()
       << "])\n";

  // Copy the data into the local process.
  int32_t workspace_descriptor;
  if (id_ == channel.owner) {
    workspace_descriptor = read(channel.address);
    write(channel.address, NotProcess);
  } else {
    unique_lock<mutex> channel_lock(server_.channels_.mutex);
    WaitingProcess process = server_.channels_.processes.at(channel);
    server_.channels_.processes.erase(channel);
    if (process.id != id_)
      throw logic_error("This instance should be the waiting one.");
    workspace_descriptor = process.workspace_descriptor;
  }

  verr << "Wdesc = " << workspace_descriptor << "\n";
  int32_t dest_address = makeWptr(workspace_descriptor) - 12;
  int32_t dest = read(dest_address);
  for (int i = 0, n = data.length(); i < n; i++) writeByte(dest + i, data[i]);

  // Wake it up.
  unique_lock<mutex> queue_lock(queue_mu_);
  schedule(workspace_descriptor, queue_lock);
  waiting_processes_--;
  on_wake_.notify_all();
}

void Instance::channelOutputDone(Channel channel) {
  verr << "instance[" << id_ << "].channelOutputDone("
       << addressString(channel.address) << ")\n";
  int32_t workspace_descriptor;
  if (id_ == channel.owner) {
    workspace_descriptor = read(channel.address);
    write(channel.address, NotProcess);
  } else {
    unique_lock<mutex> channel_lock(server_.channels_.mutex);
    WaitingProcess process = server_.channels_.processes.at(channel);
    server_.channels_.processes.erase(channel);
    if (process.id != id_)
      throw logic_error("This instance should be the waiting one.");
    workspace_descriptor = process.workspace_descriptor;
  }

  unique_lock<mutex> queue_lock(queue_mu_);
  schedule(workspace_descriptor, queue_lock);
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

  verr << "Descheduling process " << Wptr << "\n";
  unique_lock<mutex> lock(queue_mu_);
  resumeNext(lock);
  verr << "Next process: " << Wptr << "\n";
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
  write(Wptr, NoneSelected);  // Indicate that no branch yet selected.
  if (read(Wptr - 12) != Ready) {
    unique_lock<mutex> queue_lock(queue_mu_);
    alt_sleep = true;
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
 
  bool writer_waiting;
  {
    ChannelInfo& channels = server_.channels_;
    unique_lock<mutex> channel_lock(channels.mutex);
    writer_waiting = (channels.writers.count(channel) > 0);
  }

  if (writer_waiting) {
    // External channel is ready.
    write(Wptr, A);
    A = True;
  } else if (id_ != channel.owner) {
    // External channel is not ready. Signal out.
    MESSAGE(CHANNEL_DISABLE) message;
    message.actor = id_;
    message.channel = channel;

    server_.onChannelDisable(move(message));

    A = False;
  } else if (read(C) != makeWdesc(Wptr)) {
    // Internal channel is ready.
    if (read(Wptr) == NoneSelected) {
      // Channel is the first ready guard.
      write(Wptr, A);
      A = True;
    } else {
      // Another guard was ready first.
      A = False;
    }
  } else {
    // The channel is not ready. Restore it to its default value.
    write(C, NotProcess);
    A = False;
  }
}

void Instance::indirect_ENBC() {
  // Skip processing if the guard is false.
  if (!A) return;

  Channel channel = server_.process_tree_.channel(id_, C);

  bool writer_waiting;
  {
    unique_lock<mutex> channel_lock(server_.channels_.mutex);
    writer_waiting = (server_.channels_.writers.count(channel) > 0);
  }

  if (writer_waiting) {
    // External channel is ready.
    write(Wptr - 12, Ready);
  } else {
    // External channel is not ready. Signal to process server.
    MESSAGE(CHANNEL_ENABLE) message;
    message.actor = id_;
    message.channel = channel;

    server_.onChannelEnable(move(message));

    if (id_ == channel.owner) {
      // Internal channel.
      if (read(C) != makeWdesc(Wptr)) {
        // Another process is waiting on channel B.
        write(Wptr - 12, Ready);
      } else {
        // No process waiting on channel B. Wait on it.
        write(B, makeWdesc(Wptr));
      }
    }
  }
}

void Instance::indirect_IN() {
  Channel channel = server_.process_tree_.channel(id_, B);

  if (id_ == channel.owner && read(B) != NotProcess) {
    int32_t chan_value = read(B);

    // Internal channel is ready. Communication can proceed.
    int32_t source = read(makeWptr(chan_value) - 12);
    for (int i = 0; i < A; i++)
      writeByte(C + i, readByte(source + i));

    // Reset the channel.
    write(B, NotProcess);

    // Inform the server.
    MESSAGE(CHANNEL_RESET) message;
    message.actor = id_;
    message.channel = channel;
    server_.onChannelReset(move(message));

    // Reschedule the other thread.
    unique_lock<mutex> queue_lock(queue_mu_);
    waiting_processes_--;
    schedule(chan_value, queue_lock);
  } else {
    // Check if externally ready.
    bool writer_waiting;
    WaitingWriter writer;
    {
      ChannelInfo& channels = server_.channels_;
      unique_lock<mutex> channel_lock(channels.mutex);
      writer_waiting = (channels.writers.count(channel) > 0);
      if (writer_waiting) {
        // Writer is waiting. The channel will read.
        writer = move(channels.writers.at(channel));
        channels.writers.erase(channel);
      }
    }
    if (writer_waiting) {
      // Externally ready.
      for (int i = 0, n = writer.data.length(); i < n; i++)
        writeByte(C + i, writer.data[i]);
  
      // Reset the external channel.
      MESSAGE(CHANNEL_OUT_DONE) message;
      message.actor = id_;
      message.writer = writer.id;
      message.channel = channel;
  
      server_.onChannelOutputDone(move(message));
  
      // Reset the internal channel.
      if (id_ == channel.owner) write(B, NotProcess);
    } else {
      MESSAGE(CHANNEL_IN) message;
      message.actor = id_;
      message.channel = channel;

      // Write the channel info in preparation for descheduling.
      write(Wptr - 4, Iptr);
      write(Wptr - 12, C);
      if (id_ == channel.owner) write(B, makeWdesc(Wptr));

      WaitingProcess process;
      process.id = id_;
      process.workspace_descriptor = makeWdesc(Wptr);
      {
        unique_lock<mutex> channel_lock(server_.channels_.mutex);
        server_.channels_.processes.emplace(channel, process);
      }

      server_.onChannelInput(move(message));

      unique_lock<mutex> queue_lock(queue_mu_);
      waiting_processes_++;
      deschedule(queue_lock);
    }
  }
}
//  Channel channel = server_.process_tree_.channel(id_, B);
//
//  write(Wptr - 4, Iptr);
//  write(Wptr - 12, C);
//  if (id_ != channel.owner) {
//    unique_lock<mutex> queue_lock(queue_mu_);
//    waiting_processes_++;
//  }
// 
//  bool writer_waiting;
//  WaitingWriter writer;
//  {
//    ChannelInfo& channels = server_.channels_;
//    unique_lock<mutex> channel_lock(channels.mutex);
//    writer_waiting = (channels.writers.count(channel) > 0);
//    if (writer_waiting) {
//      writer = move(channels.writers.at(channel));
//      channels.writers.erase(channel);
//    } else if (id_ != channel.owner) {
//      WaitingProcess process;
//      process.id = id_;
//      process.workspace_descriptor = makeWdesc(Wptr);
//      channels.processes.emplace(channel, process);
//    }
//  }
//
//  if (writer_waiting) {
//    // External channel is ready.
//    for (int i = 0, n = writer.data.length(); i < n; i++)
//      writeByte(C + i, writer.data[i]);
//
//    // Reset the external channel.
//    MESSAGE(CHANNEL_OUT_DONE) message;
//    message.actor = id_;
//    message.writer = writer.id;
//    message.channel = channel;
//
//    server_.onChannelOutputDone(move(message));
//
//    // Reset the internal channel.
//    if (id_ == channel.owner) write(B, NotProcess);
//  } else {
//    // Signal to process server.
//    MESSAGE(CHANNEL_IN) message;
//    message.actor = id_;
//    message.channel = channel;
//
//    // onChannelInput is capable of resulting in the process being rescheduled
//    // externally. If this is yet to be handled internally, this could lead to
//    // undefined behaviour. Luckily, only one of these two situations is allowed
//    // to happen because there may only be one writer and one reader at any
//    // given time.
//    server_.onChannelInput(move(message));
//
//    if (id_ != channel.owner) {
//      // This is an external channel and it is not ready. Wait.
//      unique_lock<mutex> queue_lock(queue_mu_);
//      resumeNext(queue_lock);
//    } else {
//      // This is an internal channel.
//      unique_lock<mutex> queue_lock(queue_mu_);
//      waiting_processes_--;  // Reverse the increment done prior to onChannelInput.
//      int32_t chan_value = read(B);
//      
//      if (chan_value != NotProcess) {
//        // Internal channel is ready. Communication can proceed.
//        int32_t source = read(makeWptr(chan_value) - 12);
//        for (int i = 0; i < A; i++)
//          writeByte(C + i, readByte(source + i));
//
//        // Reset the channel.
//        write(B, NotProcess);
//
//        // Reschedule the other thread.
//        waiting_processes_--;
//        schedule(chan_value, queue_lock);
//      } else {
//        // No writer is ready. Deschedule.
//        write(B, makeWdesc(Wptr));
//
//        waiting_processes_++;
//        deschedule(queue_lock);
//      }
//    }
//  }

void Instance::indirect_OUT() {
  Channel channel = server_.process_tree_.channel(id_, B);

  if (id_ == channel.owner && read(B) != NotProcess) {
    int32_t chan_value = read(B);
    int32_t dest_address = makeWptr(chan_value) - 12;
    int32_t dest = read(dest_address);

    // Internal channel is ready. Communication can proceed.
    for (int i = 0; i < A; i++)
      writeByte(dest + i, readByte(C + i));

    // Reset the channel.
    write(B, NotProcess);

    // Inform the server.
    MESSAGE(CHANNEL_RESET) message;
    message.actor = id_;
    message.channel = channel;
    server_.onChannelReset(move(message));

    // Reschedule the other thread.
    unique_lock<mutex> queue_lock(queue_mu_);
    waiting_processes_--;
    schedule(chan_value, queue_lock);
  } else {
    MESSAGE(CHANNEL_OUT) message;
    message.actor = id_;
    message.channel = channel;
    for (int i = 0; i < A; i++)
      message.data.push_back(static_cast<char>(readByte(C + i)));

    // Write the channel info in preparation for descheduling.
    write(Wptr - 4, Iptr);
    write(Wptr - 12, C);
    if (id_ == channel.owner) write(B, makeWdesc(Wptr));

    WaitingProcess process;
    process.id = id_;
    process.workspace_descriptor = makeWdesc(Wptr);
    {
      unique_lock<mutex> channel_lock(server_.channels_.mutex);
      server_.channels_.processes.emplace(channel, process);
    }

    server_.onChannelOutput(move(message));

    unique_lock<mutex> queue_lock(queue_mu_);
    waiting_processes_++;
    deschedule(queue_lock);
  }
}
//  Channel channel = server_.process_tree_.channel(id_, B);
//
//  write(Wptr - 4, Iptr);
//  write(Wptr - 12, C);
//  {
//    unique_lock<mutex> queue_lock(queue_mu_);
//    waiting_processes_++;
//  }
//
//  bool reader_waiting;
//  WaitingReader reader;
//  {
//    ChannelInfo& channels = server_.channels_;
//    unique_lock<mutex> channel_lock(channels.mutex);
//    reader_waiting = (channels.readers.count(channel) > 0);
//    if (reader_waiting) {
//      reader = move(channels.readers.at(channel));
//      channels.readers.erase(channel);
//    } else if (id_ != channel.owner) {
//      WaitingProcess process;
//      process.id = id_;
//      process.workspace_descriptor = makeWdesc(Wptr);
//      channels.processes.emplace(channel, process);
//    }
//  }
//  
//  // Construct the message to hand to the process server.
//  MESSAGE(CHANNEL_OUT) message;
//  message.actor = id_;
//  message.channel = channel;
//  for (int i = 0; i < A; i++)
//    message.data.push_back(static_cast<char>(readByte(C + i)));
//
//  // onChannelOutput is capable of resulting in the process being rescheduled
//  // externally. If this is yet to be handled internally, this could lead to
//  // undefined behaviour. Luckily, only one of these two situations is allowed
//  // to happen because there may only be one writer and one reader at any
//  // given time.
//  server_.onChannelOutput(move(message));
//
//  if (reader_waiting || id_ != channel.owner) {
//    // The communication will have been handled by the onChannelOutput.
//    // Reset the internal channel.
//    if (id_ == channel.owner) write(B, makeWdesc(Wptr));
//
//    unique_lock<mutex> queue_lock(queue_mu_);
//    resumeNext(queue_lock);
//  } else {
//    unique_lock<mutex> queue_lock(queue_mu_);
//    waiting_processes_--;  // Undo the increment performed prior to onChannelOutput.
//    int32_t chan_value = read(B);
//    bool is_internal_ready = (chan_value != NotProcess);
//
//    if (is_internal_ready) {
//      // Internal channel is ready.
//      int32_t dest_address = makeWptr(chan_value) - 12;
//      int32_t dest = read(dest_address);
//
//      if (dest == Waiting) {
//        // A process is waiting in an ALT. Initiate communication and reschedule
//        // the process.
//        write(B, makeWdesc(Wptr));
//        write(dest_address, Ready);
//
//        // The number of waiting processes remains unaffected.
//        schedule(chan_value, queue_lock);
//        deschedule(queue_lock);
//      } else {
//        // A process is waiting directly. The communication can proceed.
//        int32_t source = read(makeWptr(chan_value) - 12);
//        for (int i = 0; i < A; i++) writeByte(C + i, readByte(source + i));
//
//        // Reset the channel.
//        write(B, NotProcess);
//
//        // Reschedule the other thread.
//        waiting_processes_--;
//        schedule(chan_value, queue_lock);
//      }
//    } else {
//      // No reader is ready. Deschedule.
//      write(B, makeWdesc(Wptr));
//
//      waiting_processes_++;
//      deschedule(queue_lock);
//    }
//  }

void Instance::indirect_RESETCH() {
  MESSAGE(CHANNEL_RESET) message;
  message.actor = id_;
  message.channel = server_.process_tree_.channel(id_, A);
  server_.onChannelReset(move(message));

  VM::indirect_RESETCH();
}
