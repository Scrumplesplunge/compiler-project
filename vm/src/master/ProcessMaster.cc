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
    Socket&& socket, int32_t data_start, string data, string bytecode)
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

ProcessMaster::ProcessMaster(const JobConfig& config)
    : bytecode_(getFileContents(config.bytecode_file)),
      metadata_(loadMetaData(config.metadata_file)) {
  // Connect to all the workers.
  for (const WorkerAddress& address : config.workers) {
    if (options::verbose)
      cerr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    socket.connect(address.host, address.port);
    workers_.push_back(make_unique<ProcessServerHandle>(
        move(socket), metadata_.memory_start, metadata_.static_data,
        bytecode_));
  }
}

void ProcessMaster::serve() {
  // Run the program on the first worker.
  // TODO: Make a way for the program to specify how much space the *initial*
  // process requires.
  workers_[0]->startInstance(metadata_.workspace_pointer, 0, metadata_.memory_size, 0);
  cin.get();
}
