#include "config.h"

#include <iostream>
#include <string>
#include <util/args.h>
#include <util/messenger.h>
#include <util/socket.h>
#include <vector>

using namespace std;

USAGE("Usage: master --job_file [filename]\n\n"
      "Start a job on the distributed system.\n");

OPTION(string, job_file, "",
       "JSON file containing the configuration for the job.");

FLAG(debug, "Show debugging information whilst running.");

void serve(vector<Messenger> workers) {
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (options::debug)
    cerr << "Parsing job file..\n";
  JobConfig config = loadConfig(options::job_file);

  if (options::debug) {
    cerr << "Bytecode file : " << config.bytecode_file << "\n"
         << "Data file     : " << config.data_file << "\n"
         << "Workers       : ";

    bool indent = false;
    for (WorkerAddress& address : config.workers) {
      if (indent) cout << string(16, ' ');
      indent = true;
      cerr << address.host << ":" << address.port << "\n";
    }
  }

  vector<Messenger> workers;
  for (WorkerAddress& address : config.workers) {
    if (options::debug)
      cerr << "Connecting to " << address.host << ":" << address.port << "..\n";
    Socket socket;
    try {
      socket.connect(address.host, address.port);
    } catch (const socket_error& error) {
      cerr << "Failed to connect to worker " << address.host << ":"
           << address.port << ". Aborting.\n";
      return 1;
    }
    workers.push_back(Messenger(move(socket)));
  }

  if (options::debug)
    cerr << "Serving..\n";
  serve(move(workers));
}
