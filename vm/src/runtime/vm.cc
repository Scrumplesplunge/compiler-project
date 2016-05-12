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
#include <util/atomic_output.h>
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
  verr << "Loading bytecode..\n";
  ifstream bytecode_file(options::bytecode_file, ios::binary);
  if (!bytecode_file) {
    cerr << "Failed to open bytecode file for reading.\n";
    return 1;
  }
  string bytecode = string(istreambuf_iterator<char>(bytecode_file),
                           istreambuf_iterator<char>());
  verr << "Bytecode size: " << bytecode.length() << "\n";

  // Load the metadata.
  verr << "Loading metadata..\n";
  MetaData metadata = loadMetaData(options::metadata_file);
  verr << "Memory required: " << metadata.root_process_size << "\n";

  verr << "Converting static data..\n";
  unique_ptr<int32_t[]> static_data(
      new int32_t[(3 + metadata.static_data.length()) / 4]);
  VM::encodeStatic(metadata.static_data, static_data.get());

  // Initialize the virtual machine.
  verr << "Creating virtual machine instance..\n";
  VM vm(VM::TopWptr - metadata.root_process_size + 4,
        metadata.root_process_size, static_data.get(),
        metadata.static_data.length(), bytecode.c_str(), bytecode.length());

  vm.set_workspace_pointer(VM::TopWptr);

  verr << "Starting VM..\n\n";

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
  verr << "Done.\n";

  if (options::summary) {
    cerr << "Total time   : "
         << formatDuration(duration_cast<nanoseconds>(end - start).count())
         << "\nClock cycles : " << vm.cycles() << "\n";
  }

  return 0;
}
