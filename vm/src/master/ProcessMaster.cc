#include "ProcessMaster.h"

#include "../util.h"
#include "../runtime/VM.h"

#include <fstream>
#include <functional>
#include <iostream>
#include <util/args.h>
#include <util/binary.h>
#include <util/stream.h>
#include <util/socket.h>

using namespace std;
using namespace std::placeholders;

ProcessServerHandle::ProcessServerHandle(
    Socket&& socket, int32_t data_start, string&& data, string bytecode)
    : messenger_(move(socket)) {
  // Send the handshake message.
  MESSAGE(START_PROCESS_SERVER) message;
  message.data_start = data_start;
  message.data = move(data);
  message.bytecode = move(bytecode);
  send(message);
}

void ProcessServerHandle::startInstance(
    int32_t workspace_pointer, int32_t instruction_pointer,
    int32_t space_needed, int32_t instance_id) {
  MESSAGE(START_INSTANCE) message;
  message.request.workspace_pointer = workspace_pointer;
  message.request.instruction_pointer = instruction_pointer;
  message.request.space_needed = space_needed;
  message.instance_id = instance_id;
  send(message);
}

ProcessMaster::ProcessMaster(const JobConfig& config) {
  // Load the bytecode.
  string bytecode = getFileContents(config.bytecode_file);

  // Set up a reader for the data file.
  ifstream data_file(config.data_file, ios::binary);
  StandardInputStream stream(data_file);
  BinaryReader reader(stream);

  // Read the data blob.
  int32_t data_start = reader.readInt32();
  int32_t data_length = reader.readInt32();
  data_end_ = data_start + data_length;
  unique_ptr<char[]> buffer(new char[data_length]);
  reader.readBytes(buffer.get(), data_length);
  string data(buffer.get(), data_length);

  // Connect to all the workers.
  for (const WorkerAddress& address : config.workers) {
    if (options::verbose)
      cerr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    socket.connect(address.host, address.port);
    workers_.push_back(make_unique<ProcessServerHandle>(
        move(socket), data_start, move(data), bytecode));
  }
}

void ProcessMaster::serve() {
  // Run the program on the first worker.
  // TODO: Make a way for the program to specify how much space the *initial*
  // process requires.
  workers_[0]->startInstance(data_end_, 0, 1024, 0);
  cin.get();
}
