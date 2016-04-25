#include "operations.h"
#include "../gen/Direct.h"
#include "../lib/util/args.h"
#include "../lib/util/table.h"

#include <fstream>
#include <functional>
#include <iostream>
#include <vector>

using namespace std;

USAGE("Usage: das --bytecode_file [filename]\n\n"
      "Disassemble Transputer bytecode into human-readable assembler.\n");

OPTION(string, bytecode_file, "", "File containing Transputer bytecode.\n");

const char hex_digits[] = "0123456789abcdef";

static string addressString(int32_t address) {
  uint32_t raw = static_cast<uint32_t>(address);
  char out[8];
  for (int i = 8; i-- > 0;) {
    out[i] = hex_digits[raw & 0xF];
    raw >>= 4;
  }
  return "0x" + string(out, 8);
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (options::bytecode_file == "") {
    cerr << "A bytecode file must be specified.\n";
    return 1;
  }

  // Open the binary file.
  ifstream source(options::bytecode_file, ios::binary);
  string bytecode = string(istreambuf_iterator<char>(source),
                           istreambuf_iterator<char>());

  // Accumulate the output in a table before producing it.
  unsigned int location_column_width = 0;
  unsigned int bytes_column_width = 0;
  unsigned int source_column_width = 0;
  int row = 0;
  Table output(5);
  output.setColumnWidth(1, 2);
  output.setColumnWidth(3, 2);

  auto add_row = [&](string location, string bytes, string source) {
    // Location.
    if (location.length() > location_column_width)
      location_column_width = location.length();
    output.setCell(row, 0, location);

    output.setCell(row, 1, "|");
    
    // Bytes.
    if (bytes.length() > bytes_column_width)
      bytes_column_width = bytes.length();
    output.setCell(row, 2, bytes);

    output.setCell(row, 3, "|");

    // Source.
    if (source.length() > source_column_width)
      source_column_width = source.length();
    output.setCell(row, 4, source);

    row++;
  };
  
  // Add the table header.
  add_row("", "Bytes:", "Source:");

  int location = 0;
  string bytes;
  int32_t operand = 0;
  int n = bytecode.length();
  for (int i = 0; i < n; i++) {
    char c = bytecode[i];

    // Extract the components.
    Direct op = static_cast<Direct>(c & 0xF0);
    int32_t argument = c & 0xF;
    operand |= argument;

    // Add the byte to the bytes field.
    if (bytes.length() > 0) bytes.push_back(' ');
    bytes.push_back(hex_digits[static_cast<unsigned int>(op) >> 4]);
    bytes.push_back(hex_digits[argument]);

    // Handle the byte semantically.
    switch (op) {
      case PFIX:
        operand = operand << 4;
        break;
      case NFIX:
        operand = ~operand << 4;
        break;
      case OPR:
        add_row(addressString(location), bytes,
                Operation(0, static_cast<Indirect>(operand)).toString());
        bytes.clear();
        operand = 0;
        location = i + 1;
        break;
      default:
        add_row(addressString(location), bytes,
                Operation(0, op, operand).toString());
        bytes.clear();
        operand = 0;
        location = i + 1;
        break;
    }
  }

  location_column_width += 1; output.setColumnWidth(0, location_column_width);
  bytes_column_width += 1;    output.setColumnWidth(2, bytes_column_width);

  output.output(cout);

  return 0;
}
