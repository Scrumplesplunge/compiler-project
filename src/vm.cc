#include "operations.h"
#include "State.h"

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
  State state(1 << 20);
  //cout << state.toString();
  while (state.Iptr < operations.size()) {
    //cout << "    \t" << operations[state.Iptr].toString() << "\n";
    state.perform(operations[state.Iptr]);
    //cout << state.toString();
  }

  return 0;
}
