#include "../runtime/VM.h"
#include "../util.h"

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

FLAG(debug, "Show debugging information whilst running.");

void serve(Socket client) {
  Messenger messenger(move(client));
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (options::debug)
    cerr << "Creating server socket..\n";
  Socket server;
  server.bind(options::host, options::port);
  if (options::debug)
    cerr << "Listening for incoming connections..\n";
  server.listen();

  while (true) {
    Socket client = server.accept();
    string hostport = client.hostPort();
    if (options::debug)
      cerr << "Accepted connection from " << hostport << ".\n";
    serve(move(client));
    if (options::debug)
      cerr << "Stopped serving " << hostport << ".\n";
  }
}

