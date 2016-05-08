#include "ProcessServer.h"

#include <iostream>
#include <util/args.h>
#include <util/messenger.h>
#include <util/socket.h>

using namespace std;

USAGE("Usage: worker --host [host IP] --port [host port]\n\n"
      "Spawn a process which acts as a worker node for any connecting "
      "master.\n");

OPTION(string, host, "0.0.0.0",
       "Address to bind the worker server to.");
OPTION(int, port, 17994,
       "Port to bind the worker server to.");

void serve(Socket socket) {
  if (options::verbose)
    cerr << "Constructing Process Server instance..\n";
  ProcessServer server(move(socket));
  if (options::verbose)
    cerr << "Serving..\n";
  server.serve();
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (options::verbose)
    cerr << "Creating server socket..\n";
  Socket server;
  server.bind(options::host, options::port);
  if (options::verbose)
    cerr << "Listening for incoming connections..\n";
  server.listen();

  while (true) {
    Socket socket = server.accept();
    string hostport = socket.hostPort();
    if (options::verbose)
      cerr << "Accepted connection from " << hostport << ".\n";
    serve(move(socket));
    if (options::verbose)
      cerr << "Stopped serving " << hostport << ".\n";
  }
}

