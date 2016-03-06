#include "operations.h"
#include "VM.h"

#include <ctype.h>
#include <iostream>
#include <map>
#include <regex>
#include <string>
#include <vector>

using namespace std;

int main() {
  // Contains the instructions from the input.
  vector<Operation> operations;

  if (!parseOperations(cin, &operations)) return 1;

  // Output the generated code.
  for (Operation& op : operations) {
    //cout << op.toString() << "\n";
  }

  // Run it!
  VM vm(1 << 20);
  //cerr << vm.toString();
  while (vm.Iptr < operations.size()) {
    //cerr << "   " << operations[vm.Iptr].toString() << "\n";
    vm.perform(operations[vm.Iptr]);
    //cerr << vm.toString();
  }

  return 0;
}
