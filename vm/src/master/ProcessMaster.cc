#include "ProcessMaster.h"

#include "../util.h"

#include <fstream>
#include <functional>
#include <util/args.h>
#include <util/socket.h>

using namespace std;
using namespace std::placeholders;

ProcessServerHandle::ProcessServerHandle(
    Socket&& socket, string data, string bytecode)
    : messenger_(move(socket)) {
  // Send the handshake message.
  MESSAGE(START_PROCESS_SERVER) message;
  message.data = move(data);
  message.bytecode = move(bytecode);
  messenger_.SEND(message);
}

ProcessMaster::ProcessMaster(const JobConfig& config) {
  // Load the bytecode and data.
  string data = getFileContents(config.data_file);
  string bytecode = getFileContents(config.bytecode_file);

  // Connect to all the workers.
  for (const WorkerAddress& address : config.workers) {
    if (options::verbose)
      cerr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    socket.connect(address.host, address.port);
    workers_.push_back(
        make_unique<ProcessServerHandle>(move(socket), data, bytecode));
  }
}

void ProcessMaster::serve() {}
