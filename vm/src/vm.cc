#include "operations.h"
#include "../lib/util/args.h"
#include "../lib/util/binary.h"
#include "runtime/VM.h"

#include <fstream>
#include <iostream>
#include <memory>
#include <stdint.h>
#include <string>

using namespace std;

USAGE("Usage: vm --bytecode [filename] --data [filename]\n\n"
      "Execute a program using the virtual machine.\n");

OPTION(string, bytecode, "",
       "File containing the application bytecode to be executed.");
OPTION(string, data, "",
       "File containing the application data. This will be used to initialize "
       "the RAM of the virtual machine before the program is started.");

int main(int argc, char* args[]) {
  args::process(&argc, &args);
  if (options::bytecode == "") {
    cerr << "A bytecode file must be specified.\n";
    return 1;
  }

  // Construct the program memory.
  int memory_size = 1 << 20;
  unique_ptr<int32_t[]> memory(new int32_t[memory_size / 4]);

  // Open the bytecode file and load the contents.
  ifstream bytecode_file(options::bytecode, ios::binary);
  string bytecode = string(istreambuf_iterator<char>(bytecode_file),
                           istreambuf_iterator<char>());

  // Initialize the virtual machine.
  VM vm(move(memory), memory_size, bytecode);

  if (options::data != "") {
    // Open the data file and load the contents.
    ifstream data_file(options::data, ios::binary);
    BinaryReader reader(data_file);

    int32_t address = reader.readInt32();
    int32_t length = reader.readInt32();
    int buffer_size = 2 * length;
    unique_ptr<char[]> buffer(new char[buffer_size]);
    reader.readBytes(buffer.get(), length);

    while (reader.isGood()) {
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

  // Run the program.
  vm.run();

  return 0;
}
