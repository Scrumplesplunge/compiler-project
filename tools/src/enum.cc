/*
  This script takes a file of the form:

  -- Simple.enum --
    A      # Foo
    B(2)   # Bar
    C      # Baz
  -----------------

  And produces a header:

  -- simple.h --
    #include <string>

    namespace Simple {

    enum Simple {
      A,      // Foo
      B = 2,  // Bar
      C,      // Baz
    };

    std::string toString(Simple value);
    bool fromString(std::string value, Simple* output);
  --------------

  And the corresponding implementation. This is because I am tired of repeatedly
  solving the same problem when I want to be able to use the string-names of
  enum values for input/output.
*/

#include <fstream>
#include <iostream>
#include <regex>
#include <stdint.h>
#include <vector>

using namespace std;

struct Item {
  string name;
  int64_t value;
  string description;
};

int main(int argc, char* args[]) {
  if (argc != 3) {
    cerr << "Usage: enum <input file> <enum name>\n"
            "\n"
            "  input file  - Name of the enum file to process.\n"
            "  output name - Name to assign to the generated enum. This will "
                            "be used for both the header and source file "
                            "names.\n";
    return 1;
  }

  string input_file = args[1];
  string output_name = args[2];

  // Verify that the output name is a valid enumeration name.
  regex enum_name_syntax(R"([A-Za-z][A-Za-z0-9_]*)");
  if (!regex_match(output_name, enum_name_syntax)) {
    cerr << "Invalid enumeration name '" << output_name << "'.\n";
    return 1;
  }

  // Open the input file.
  ifstream input(input_file);
  if (!input.good()) {
    cerr << "Failed to open input file '" << input_file << "'.\n";
    return 1;
  }

  // Generate the enum mapping.
  vector<Item> enumeration;

  int line_number = 0;  // Incremented on each line read, for error messages.
  int64_t index = 0;    // Incremented for each item, reset by explicit values.
  string line;
  // 1 = Enumeration item.
  // 2 = Identifier.
  // 3 = Explicit value parantheses (optional).
  // 4 = Explicit value.
  // 5 = Description (optional).
  // 6 = Description text.
  regex line_syntax(
      //    1
      R"(\s*()"
          // 2                     2
          R"(([A-Za-z][A-Za-z0-9_]*)\s*)"
          // 3     4                               4     3
          R"((\(\s*(0|[1-9][0-9]*|0[xX][0-9A-Fa-f]+)\s*\))?)"
      // 1
      R"()?)"
      //    5    6  65
      R"(\s*(#\s*(.*))?)");
  int max_enum_name_length = 0;  // Longest enumeration item name length.
  int max_enum_id_length = 0;    // Longest decimal representation of an id.
  while (getline(input, line)) {
    line_number++;
    // Match the line to the syntax pattern.
    smatch result;
    if (!regex_match(line, result, line_syntax)) {
      cerr << input_file << ": Syntax error on line " << line_number << "\n  "
           << line << "\n";
      return 1;
    }
    if (result[1].length() != 0) {
      // There is an enumeration item on this line.
      Item item;
      item.name = result[2];

      // If an explicit index was set, use it.
      if (result[3].length() != 0) index = stoll(result[4], 0, 0);
      item.value = index++;

      // If a description was provided, set it.
      if (result[5].length() != 0) item.description = result[6];
      enumeration.push_back(item);

      // Update the maximum length values.
      if (item.name.length() > max_enum_name_length)
        max_enum_name_length = item.name.length();
      int id_length = to_string(item.value).length();
      if (id_length > max_enum_id_length)
        max_enum_id_length = id_length;
    }
  }

  // Construct the header file.
  ofstream output(output_name + ".h");
  if (!output.good()) {
    cerr << "Failed to open header file '" << output_name
         << ".h' for output.\n";
    return 1;
  }

  output << "// AUTOMATICALLY GENERATED CODE - DO NOT EDIT.\n"
            "\n"
            "#pragma once\n"
            "\n"
            "#include <string>\n"
            "\n"
            "enum " << output_name << " {\n";
  for (auto item : enumeration) {
    output << "  " << item.name
           << string(max_enum_name_length - item.name.length(), ' ')
           << " = " << item.value << ",";
    if (item.description.length() > 0) {
      output << string(max_enum_id_length - to_string(item.value).length(), ' ')
             << "  // " << item.description;
    }
    output << "\n";
  }
  output << "};\n"
            "\n"
            "std::string toString(" << output_name << " value);\n"
            "bool fromString("
                "const std::string& value, " << output_name << "* output);\n"
            "\n";
  output.close();
  cout << output_name << ".h generated.\n";

  // Construct the source file.
  output.open(output_name + ".cc");
  if (!output.good()) {
    cerr << "Failed to open source file '" << output_name
         << ".cc' for output.\n";
    return 1;
  }

  output << "// AUTOMATICALLY GENERATED CODE - DO NOT EDIT.\n"
            "\n"
            "#include \"" << output_name << ".h\"\n"
            "\n"
            "#include <map>\n"
            "\n"
            "using namespace std;\n"
            "\n"
            "string toString(" << output_name << " value) {\n"
            "  switch (value) {\n";
  for (auto item : enumeration) {
    output << "    case " << item.name << ": "
           << string(max_enum_name_length - item.name.length(), ' ')
           << "return \"" << item.name << "\";\n";
  }
  output << "    default: return \"<INVALID>\";\n"
            "  }\n"
            "}\n"
            "\n"
            "bool fromString("
                "const string& value, " << output_name << "* output) {\n"
            "  static const map<string, " << output_name << "> lookup = {\n";
  for (auto item : enumeration) {
    output << "    {\"" << item.name << "\", "
           << string(max_enum_name_length - item.name.length(), ' ')
           << item.name << "},\n";
  }
  output << "  };\n"
            "  auto i = lookup.find(value);\n"
            "  if (i == lookup.end()) return false;\n"
            "  *output = i->second;\n"
            "  return true;\n"
            "}\n";
  cout << output_name << ".cc generated.\n";
}
