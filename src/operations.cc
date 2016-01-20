#include "operations.h"
#include "../../tools/src/string.h"

#include <iostream>
#include <regex>
#include <sstream>

using namespace std;

Operation::Operation() {
  // Default instruction is a no-op.
  type_ = DIRECT;
  direct_ = ADC;
  operand_ = 0;
}

Operation::Operation(Direct op, int32_t argument) {
  type_ = DIRECT;
  direct_ = op;
  operand_ = argument;
}

Operation::Operation(Indirect op) {
  type_ = INDIRECT;
  indirect_ = op;
}

Operation::Operation(Unit op) {
  type_ = UNIT;
  unit_ = op;
}

bool Operation::fromString(string operation, Operation* output) {
  // No operand => indirect or unit operation.
  if (::fromString(toUpper(operation), &output->indirect_)) {
    output->type_ = INDIRECT;
    return true;
  } else if (::fromString(toUpper(operation), &output->unit_)) {
    output->type_ = UNIT;
    return true;
  } else {
    return false;
  }
}

bool Operation::fromString(
    string operation, int32_t argument, Operation* output) {
  // Operand => direct operation.
  if (!::fromString(toUpper(operation), &output->direct_)) return false;

  output->operand_ = argument;
  output->type_ = DIRECT;
  return true;
}

string Operation::toString() const {
  switch (type_) {
    case DIRECT:
      return toLower(::toString(direct_)) + " " +
             to_string(static_cast<int32_t>(operand_));
    case INDIRECT:
      return toLower(::toString(indirect_));
    case UNIT:
      return toLower(::toString(unit_));
  }
}

// Instruction pattern.
const char pattern_string[] =
  //     12          2 1
  R"(^\s*(([A-Z0-9_]+):)?)"                      // Line label (if any)
    //    34         45   6
    R"(\s*(([a-z0-9]+)(\s+()"                  // Mnemonic, optional:
      R"(-?0x[0-9a-fA-F]+|)"                     // Hexadecimal argument
      R"(-?[1-9][0-9]*|)"                        // Decimal argument
      R"(-?[0-7]+|)"                             // Octal argument
      // 7          78       9          98
      R"(([A-Z0-9_]+)(\s*-\s*([A-Z0-9_]+))?)"  // Symbolic argument
    // 65 3
    R"())?)?)"                                 // end of optional
  //    A   A
  R"(\s*(#.*)?$)";                             // Comment

// Defined labels.
typedef map<string, int> Environment;

class Reference {
 public:
  Reference(int line_number, int operation, string symbol)
      : line_number_(line_number), operation_(operation), symbol1_(symbol),
        has_symbol2_(false) {}

  Reference(
      int line_number, int operation, string symbol1, string symbol2)
      : line_number_(line_number), operation_(operation), symbol1_(symbol1),
        has_symbol2_(true), symbol2_(symbol2) {}

  // Apply the reference.
  bool apply(vector<Operation>& operations, const Environment& env) {
    if (env.count(symbol1_) == 0) {
      cerr << "Error: Use of undefined label " << symbol1_ << " at line "
           << line_number_ << ".\n";
      return false;
    }

    int operand = env.at(symbol1_);

    if (has_symbol2_) {
      if (env.count(symbol2_) == 0) {
        cerr << "Error: Use of undefined label " << symbol2_ << " at line "
             << line_number_ << ".\n";
        return false;
      }
      operand -= env.at(symbol2_);
    }

    if (!operations[operation_].setOperand(operand)) {
      cerr << "Programmer Error: Tried to assign operand value on operation "
              "with no operand.\n";
      cerr << "Operation: " << operations[operation_].toString() << "\n"
           << "Line: " << line_number_ << "\n";
      return false;
    }

    return true;
  }

 private:
  int line_number_;

  int operation_;
  string symbol1_;

  bool has_symbol2_ = false;
  string symbol2_;
};

bool parseOperations(istream& input, vector<Operation>* output) {
  // Contains all labels referenced in the input. The value of the label is
  // a location for that label.
  Environment labels;

  // Contains all lines with the variables they reference.
  vector<Reference> references;

  // Regex matcher for each line:
  regex pattern(pattern_string);

  int line_number = 0;
  string line;
  while (getline(input, line)) {
    line_number++;
    smatch matches;
    regex_match(line, matches, pattern);

    if (matches.empty()) {
      // Line did not match the expected format.
      cerr << "Syntax error on line " << line_number << ".\n";
      return false;
    }

    string label = matches.str(2),
           mnemonic = matches.str(4),
           argument = matches.str(6),
           symbol1 = matches.str(7),
           symbol2 = matches.str(9);

    // Handle the line label, if there is one.
    if (label.length() > 0) {
      if (labels.count(label) > 0) {
        // Duplicate label.
        cerr << "Error: Label " << label << " is redefined at line "
             << line_number << ".\n";
        return false;
      }

      labels[label] = output->size();
    }

    // Handle the operation, if there is one.
    if (mnemonic.length() > 0) {
      // Put the operation into the list.
      Operation op;

      if (argument.length() > 0) {
        int operand;

        // Evaluate the operand, if possible.
        if (symbol1.length() > 0) {
          if (symbol2.length() > 0) {
            // Symbolic expression: FOO - BAR
            references.push_back(
                Reference(line_number, output->size(), symbol1, symbol2));
          } else {
            // Symbolic argument: FOO
            references.push_back(
                Reference(line_number, output->size(), symbol1));
          }
          // Set the operation with a placeholder argument.
          operand = 0;
        } else {
          // Numeric argument.
          operand = stoll(argument, nullptr, 0);
        }
     
        // Set the operation.
        if (!Operation::fromString(mnemonic, operand, &op)) {
          cerr << "Error: Bad instruction at line " << line_number << ".\n";
          return false;
        }
      } else {
        // The operation has no argument.
        if (!Operation::fromString(mnemonic, &op)) {
          cerr << "Error: Bad instruction at line " << line_number << ".\n";
          return false;
        }
      }

      output->push_back(op);
    }
  }

  // Go through and apply all references and apply them.
  for (Reference& reference : references) {
    if (!reference.apply(*output, labels)) return 1;
  }

  return true;
}
