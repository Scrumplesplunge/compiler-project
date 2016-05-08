#include "../network.h"
#include "config.h"
#include "ProcessMaster.h"

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

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  // Open the configuration file.
  if (options::verbose)
    cerr << "Parsing job file..\n";
  JobConfig config = loadConfig(options::job_file);

  if (options::verbose) {
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

  if (options::verbose)
    cerr << "Initializing Process Master..\n";
  ProcessMaster master(config);

  if (options::verbose)
    cerr << "Serving..\n";
  master.serve();
}
