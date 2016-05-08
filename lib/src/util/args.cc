#include "args.h"
#include "table.h"
#include "text.h"

#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <map>

using namespace std;

FLAG(help, "Show this help message.");
FLAG(defaults, "List the default values for all special arguments.");
FLAG(debug, "Enable debugging features, if present.");
FLAG(verbose, "Print additional information to stderr whilst running.");

namespace args {
namespace {

typedef map<string, GenericOption*> OptionRegistry;
// Registry containing all active command-line options.
OptionRegistry* registry() {
  static OptionRegistry option_registry;
  return &option_registry;
};

// Program usage as displayed when invoked with --help.
const char* description;

}  // namespace

const char* registerDescription(const char* text) {
  description = text;
  return description;
}

void registerOption(GenericOption* argument) {
  registry()->emplace(argument->name(), argument);
}

void process(int* argc, char** args[]) {
  int j = 0;
  for (int i = 0; i < *argc; i++) {
    const char* argument = (*args)[i];
    if (!(argument[0] == '-' && argument[1] == '-')) {
      // This is not an option name. Copy the argument into the resulting
      // location.
      (*args)[j++] = (*args)[i];
      continue;
    }
    // Strip the "--" from the option name.
    string option_name = argument + 2;
    auto option = registry()->find(option_name);
    if (option == registry()->end()) {
      // Unrecognised option.
      cerr << "Unrecognised option --" << option_name << ".\n";
      exit(1);
    }
    // Parse the option.
    int args_read = 0;
    if (!option->second->read(*argc - i, *args + i, &args_read)) {
      cerr << "Bad value for option --" << option_name << ".\n";
      exit(1);
    }
    i += args_read;
  }
  // Update the size of the argument list to reflect how many resultant
  // arguments there are.
  *argc = j;

  // Handle special options.
  if (options::help || options::defaults) {
    // Show the usage information.
    if (description) {
      arrange(cout, description, 0, 80);
      cout << "\n";
    }

    // Construct the arguments table.
    Table table(4);
    table.setColumnWidth(0, 2);
    int argument_width = 0;
    int value_width = 0;
    int row = 0;
    for (const auto& option : *registry()) {
      if (static_cast<int>(option.first.length()) > argument_width)
        argument_width = option.first.length();
      table.setCell(row, 1, "--" + option.first);
      if (options::defaults) {
        // Show the default values.
        string value = string("= ") + option.second->default_value();
        if (static_cast<int>(value.length()) > value_width)
          value_width = value.length();
        table.setCell(row, 2, value);
      }
      if (options::help) {
        // Show the option descriptions.
        table.setCell(row, 3, option.second->description());
      }
      row++;
    }
    table.setColumnWidth(1, argument_width + 3);
    table.setColumnWidth(2, value_width + 2);
    table.setColumnWidth(3, 80 - argument_width - value_width - 7);
    table.output(cout);
    exit(0);
  }
}

}  // namespace args
