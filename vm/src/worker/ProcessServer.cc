#include "ProcessServer.h"

#include <functional>
#include <util/args.h>

using namespace std;
using namespace std::placeholders;

ProcessServer::ProcessServer(Socket&& socket)
    : messenger_(move(socket)) {
  if (options::verbose)
    cerr << "Configuring messenger channel..\n";
  messenger_.ON(START_PROCESS_SERVER,
                bind(&ProcessServer::onStartProcessServer, this, _1));
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
  data_ = move(message.data);
  bytecode_ = move(message.bytecode);
  is_ready_ = true;
}
