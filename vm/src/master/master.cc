#include "../network.h"
#include "../util.h"
#include "config.h"
#include "ProcessMaster.h"

#include <chrono>
#include <iostream>
#include <string>
#include <util/args.h>
#include <util/messenger.h>
#include <util/socket.h>
#include <vector>

using namespace std;
using namespace std::chrono;

USAGE("Usage: master --job_file [filename]\n\n"
      "Start a job on the distributed system.\n");

OPTION(string, job_file, "",
       "JSON file containing the configuration for the job.");

FLAG(summary, "Show a performance summary upon completion.");

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  // Open the configuration file.
  if (options::verbose)
    cerr << "Parsing job file..\n";
  JobConfig config = loadConfig(options::job_file);

  if (options::verbose) {
    cerr << "Bytecode file : " << config.bytecode_file << "\n"
         << "Data file     : " << config.metadata_file << "\n"
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
    cerr << "Starting..\n";

  high_resolution_clock::time_point start = high_resolution_clock::now();
  master.serve();
  high_resolution_clock::time_point end = high_resolution_clock::now();

  if (options::verbose)
    cerr << "Done.\n";

  if (options::summary) {
    cerr << "Total time   : "
         << formatDuration(duration_cast<nanoseconds>(end - start).count())
         << "\n";
  }
}
