#include "../util.h"
#include "VM.h"

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

USAGE("Usage: vm --bytecode_file [filename] --data_file [filename]\n\n"
      "Execute a program using the virtual machine.\n");

OPTION(string, bytecode_file, "",
       "File containing the application bytecode to be executed.");
OPTION(string, data_file, "",
       "File containing the application data. This will be used to initialize "
       "the RAM of the virtual machine before the program is started.");

FLAG(debug, "Show debugging information whilst running.");
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

  // Open the bytecode file and load the contents.
  if (options::debug)
    cerr << "Loading bytecode..\n";
  ifstream bytecode_file(options::bytecode_file, ios::binary);
  if (!bytecode_file) {
    cerr << "Failed to open bytecode file for reading.\n";
    return 1;
  }
  string bytecode = string(istreambuf_iterator<char>(bytecode_file),
                           istreambuf_iterator<char>());
  if (options::debug)
    cerr << "Bytecode size: " << bytecode.length() << "\n";

  // Construct the program memory.
  int memory_size = 1 << 20;
  if (options::debug)
    cerr << "Constructing memory buffer of size " << memory_size << "..\n";
  unique_ptr<int32_t[]> memory(new int32_t[memory_size / 4]);

  // Initialize the virtual machine.
  if (options::debug)
    cerr << "Creating virtual machine instance..\n";
  VM vm(VM::MostNeg, move(memory), memory_size, bytecode.c_str(),
        bytecode.length());

  if (options::data_file != "") {
    // Open the data file and load the contents.
    if (options::debug)
      cerr << "Loading data file..\n";
    ifstream data_file(options::data_file, ios::binary);
    if (!data_file) {
      cerr << "Failed to open data file for reading.\n";
      return 1;
    }
    StandardInputStream data_stream(data_file);
    BinaryReader reader(data_stream);

    int32_t address = reader.readInt32();
    int32_t length = reader.readInt32();
    int buffer_size = 2 * length;
    unique_ptr<char[]> buffer(new char[buffer_size]);
    reader.readBytes(buffer.get(), length);

    while (!data_file.eof()) {
      if (options::debug) {
        cerr << "Loading blob " << addressString(address) << ", size " << length
             << "\n";
      }
      // Copy the blob into the main memory of the VM.
      for (int32_t i = 0; i < length; i++)
        vm.writeByte(address + i, buffer[i]);

      // Load the next blob.
      address = reader.readInt32();
      length = reader.readInt32();
      if (length > buffer_size) {
        buffer_size = 2 * length;
        buffer.reset(new char[buffer_size]);
      }
      reader.readBytes(buffer.get(), length);
    }
  }

  if (options::debug) cerr << "Starting VM..\n\n";
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
  if (options::debug) cerr << "Done.\n";

  if (options::summary) {
    cerr << "Total time   : "
         << formatDuration(duration_cast<nanoseconds>(end - start).count())
         << "\nClock cycles : " << vm.cycles() << "\n";
  }

  return 0;
}
