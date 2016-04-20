#include "operations.h"
#include "../lib/util/args.h"
#include "runtime/VM.h"

#include <fstream>
#include <iostream>
#include <memory>
#include <stdint.h>
#include <string>

using namespace std;

OPTION(string, bytecode, "", "File containing the application bytecode.");
OPTION(string, data, "", "File containing the application data.");

int main(int argc, char* args[]) {
  args::process(&argc, &args);
  if (options::bytecode == "") {
    cerr << "A bytecode file must be specified.\n";
    return 1;
  }

  // Open the bytecode file and load the contents.
  ifstream bytecode_file(options::bytecode, ios::binary);
  string bytecode = string(istreambuf_iterator<char>(bytecode_file),
                           istreambuf_iterator<char>());

  // Construct the program memory.
  int memory_size = 1 << 20;
  unique_ptr<int32_t[]> memory(new int32_t[memory_size / 4]);

  if (options::data != "") {
    // TODO: Construct the program memory.
    cerr << "Warning: Data initialization is not implemented.\n";
    return 1;
  }

  // Run the virtual machine.
  VM vm(move(memory), memory_size, bytecode);
  vm.run();

  return 0;
}
