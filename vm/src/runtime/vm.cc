#include "../util.h"
#include "VM.h"
#include "metadata.h"

#include <chrono>
#include <fstream>
#include <iostream>
#include <math.h>
#include <memory>
#include <stdint.h>
#include <string>
#include <util/args.h>
#include <util/binary.h>

using namespace std::chrono;
using namespace std;

USAGE("Usage: vm --bytecode_file [filename] --metadata_file [filename]\n\n"
      "Execute a program using the virtual machine.\n");

OPTION(string, bytecode_file, "",
       "File containing the application bytecode to be executed.");
OPTION(string, metadata_file, "",
       "File containing the application data and configuration parameters. "
       "This will be used to initialize the RAM of the virtual machine before "
       "the program is started.");

FLAG(step, "Run the program step-by-step.");
FLAG(summary, "Show a performance summary upon completion.");

static string units(int value, string unit) {
  return to_string(value) + " " + unit + (value == 1 ? "" : "s");
}

// Format a duration as a string.
static string formatDuration(int64_t ns) {
  int64_t us = ns / 1000; ns %= 1000;
  int64_t ms = us / 1000; us %= 1000;
  int64_t s = ms / 1000;  ms %= 1000;
  int64_t m = s / 60;     s %= 60;
  int64_t h = m / 60;     m %= 60;

  if (h > 0)
    return units(h, "hour") + " and " + units(m, "minute");
  if (m > 0)
    return units(m, "minute") + " and " + units(s, "second");
  if (s > 0)
    return units(s, "second") + " and " + units(ms, "millisecond");
  if (ms > 0)
    return units(ms, "millisecond") + " and " + units(us, "microsecond");
  if (us > 0)
    return units(us, "microsecond") + " and " + units(ns, "nanosecond");
  if (ns > 0)
    return units(ns, "nanosecond");
  return "0 seconds";
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);
  if (options::bytecode_file == "") {
    cerr << "A bytecode file must be specified.\n";
    return 1;
  }
  if (options::metadata_file == "") {
    cerr << "A metadata file must be specified.\n";
    return 1;
  }

  // Open the bytecode file and load the contents.
  if (options::verbose)
    cerr << "Loading bytecode..\n";
  ifstream bytecode_file(options::bytecode_file, ios::binary);
  if (!bytecode_file) {
    cerr << "Failed to open bytecode file for reading.\n";
    return 1;
  }
  string bytecode = string(istreambuf_iterator<char>(bytecode_file),
                           istreambuf_iterator<char>());
  if (options::verbose)
    cerr << "Bytecode size: " << bytecode.length() << "\n";

  // Load the metadata.
  if (options::verbose)
    cerr << "Loading metadata..\n";
  MetaData metadata = loadMetaData(options::metadata_file);

  if (options::verbose)
    cerr << "Memory required: " << metadata.memory_size << "\n";

  // Initialize the virtual machine.
  if (options::verbose)
    cerr << "Creating virtual machine instance..\n";
  VM vm(metadata.memory_start, metadata.memory_size, bytecode.c_str(),
        bytecode.length());

  vm.set_workspace_pointer(metadata.workspace_pointer);

  // Copy the static data into the main memory of the VM.
  int32_t i = 0;
  for (char c : metadata.static_data) {
    vm.writeByte(metadata.memory_start + i, c);
    i++;
  }

  if (options::verbose)
    cerr << "Starting VM..\n\n";

  high_resolution_clock::time_point start = high_resolution_clock::now();
  if (options::debug || options::step) {
    // Step through the program.
    vm.begin();

    while (vm.running()) {
      vm.step(true);

      // Wait for input between steps if manually stepping.
      if (options::step) cin.get();
    }
  } else {
    // Run the program to completion.
    vm.run();
  }
  high_resolution_clock::time_point end = high_resolution_clock::now();
  if (options::verbose) cerr << "Done.\n";

  if (options::summary) {
    cerr << "Total time   : "
         << formatDuration(duration_cast<nanoseconds>(end - start).count())
         << "\nClock cycles : " << vm.cycles() << "\n";
  }

  return 0;
}
