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
}

void ProcessServer::serve() {
  try {
    messenger_.serve();
  } catch (...) {
    if (options::verbose)
      cerr << "Messenger closed. Shutting down.\n";
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
  data_start_ = message.data_start;
  string data(move(message.data));
  data_end_ = data_start_ + data.length();
  data_.reset(new int32_t[(data.length() + 3) / 4]);
  memcpy(data_.get(), data.c_str(), data.length());
  
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
         << "Size        : " << message.descriptor.space_needed << "\n";
  }
  thread([this, message] {
    // Create the VM instance.
    if (options::verbose)
      cerr << "Constructing instance..\n";
    Instance instance(
        message.descriptor.workspace_pointer,
        4 * message.descriptor.space_needed,
        bytecode_.c_str(), bytecode_.length(), data_start_, data_.get(),
        data_end_);

    instance.set_workspace_pointer(message.descriptor.workspace_pointer);
    instance.set_instruction_pointer(message.descriptor.instruction_pointer);

    // Run it.
    if (options::verbose)
      cerr << "Running..\n";
    instance.run();
    if (options::verbose)
      cerr << "Instance " << message.id << " terminated.\n";

    MESSAGE(INSTANCE_EXITED) exit_message;
    exit_message.id = message.id;
    messenger_.send(INSTANCE_EXITED, exit_message);
  }).detach();
}
