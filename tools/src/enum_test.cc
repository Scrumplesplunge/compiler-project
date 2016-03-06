#include "Foo.h"

#include <iostream>
#include <string>

using namespace std;

int main() {
  string line;
  cout << ">>> ";
  while (getline(cin, line)) {
    Foo temp;
    if (!fromString(line, &temp)) {
      cout << ":(\n>>> ";
      continue;
    }
    cout << "= " << temp << " (" << toString(temp) << ")\n>>> ";
  }
}
