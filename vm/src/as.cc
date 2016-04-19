#include "operations.h"

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

int main() {
  // Parse the assembler into a sequence of (possibly indirect) operations.
  vector<Operation> operations;
  Environment labels;
  vector<Reference> references;

  if (!parseOperations(cin, &operations, &labels, &references)) return 1;

  // Convert all indirect operations into calls to OPR, all long arguments into
  // sequences of PFIX and NFIX instructions, and update all jump values
  // accordingly.
  if (!encodeOperations(operations, labels, references, &cout)) return 1;

  return 0;
}
