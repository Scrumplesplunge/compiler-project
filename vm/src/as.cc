#include "operations.h"
#include "../lib/util/args.h"

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

USAGE("Usage: as --source [filename] --bytecode [filename]\n\n"
      "Assemble human-readable Transputer assembler into Transputer "
      "bytecode.\n");

OPTION(string, source, "", "File containing the assembler code.");
OPTION(string, bytecode, "", "File to store the resultant binary into.");

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (options::source == "") {
    cerr << "A source file must be specified.\n";
    return 1;
  }

  if (options::bytecode == "") {
    cerr << "An output file must be specified.\n";
    return 1;
  }

  // Open the input and output files.
  ifstream source(options::source);
  ofstream output(options::bytecode, ios::binary);

  // Parse the assembler into a sequence of (possibly indirect) operations.
  vector<Operation> operations;
  Environment labels;
  vector<Reference> references;

  if (!parseOperations(source, &operations, &labels, &references)) return 1;

  // Convert all indirect operations into calls to OPR, all long arguments into
  // sequences of PFIX and NFIX instructions, and update all jump values
  // accordingly.
  if (!encodeOperations(operations, labels, references, &output)) return 1;

  return 0;
}
