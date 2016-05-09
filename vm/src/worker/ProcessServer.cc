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
  #define ON(name, handler)  \
    on<MESSAGE(name)>(name, handler)
  messenger_.ON(START_PROCESS_SERVER,
                bind(&ProcessServer::onStartProcessServer, this, _1));
  messenger_.ON(START_INSTANCE,
                bind(&ProcessServer::onStartInstance, this, _1));
  #undef ON
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
    cerr << ::toString(message.type) << " received.\n"
         << "Data Blob Size : " << message.data.length() << "\n"
         << "Bytecode Size  : " << message.bytecode.length() << "\n";
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
         << "Instance ID : " << message.instance_id << "\n"
         << "Workspace   : " << message.request.workspace_pointer << "\n"
         << "Instruction : " << message.request.instruction_pointer << "\n"
         << "Size        : " << message.request.space_needed << "\n";
  }
  thread([this, message] {
    // Create the VM instance.
    if (options::verbose)
      cerr << "Constructing instance..\n";
    Instance instance(
        message.request.workspace_pointer, 4 * message.request.space_needed,
        bytecode_.c_str(), bytecode_.length(), data_start_, data_.get(),
        data_end_);

    instance.set_workspace_pointer(message.request.workspace_pointer);
    instance.set_instruction_pointer(message.request.instruction_pointer);

    // Run it.
    if (options::verbose)
      cerr << "Running..\n";
    instance.run();
    if (options::verbose)
      cerr << "Instance " << message.instance_id << " terminated.\n";
  }).detach();
}
