#include "operations.h"
#include "../../tools/src/string.h"

#include <iostream>
#include <regex>
#include <sstream>

using namespace std;

Operation::Operation(int location) {
  // Default instruction is a no-op.
  type_ = DIRECT;
  location_ = location;
  direct_ = ADC;
  operand_ = 0;
}

Operation::Operation(int location, Direct op, int32_t argument) {
  type_ = DIRECT;
  location_ = location;
  direct_ = op;
  operand_ = argument;
}

Operation::Operation(int location, Indirect op) {
  type_ = INDIRECT;
  location_ = location;
  indirect_ = op;
}

Operation::Operation(int location, Unit op) {
  type_ = UNIT;
  location_ = location;
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

bool Operation::fromByte(uint8_t byte, Operation* output) {
  output->type_ = DIRECT;
  output->direct_ = static_cast<Direct>(byte & 0xF0);
  output->operand_ = byte & 0xF;
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
    default:
      throw logic_error("Invalid operation type.");
  }
}

// Given a direct instruction and its operand, compute any necessary prefix
// instructions and return the full byte representation of the instruction.
string makeBytes(Direct operation, int32_t operand) {
  bool negative = (operand < 0);
  uint32_t value = static_cast<uint32_t>(operand);
  char final_operand = value & 0xF;

  // Compute the prefix backwards into the tail-end of this array.
  char prefix[8];
  int i = 8;

  // If the prefix is negative, invert the bits at the last stage.
  if (negative) {
    value = (~value) >> 4;
    prefix[--i] = NFIX | (value & 0xF);
  }
  value >>= 4;

  // Repeatedly prefix the remaining bits.
  while (value != 0) {
    prefix[--i] = PFIX | (value & 0xF);
    value >>= 4;
  }

  // Construct the full instruction.
  string output(prefix + i, 8 - i);
  output.push_back(operation | final_operand);
  return output;
}

string Operation::toBytes() const {
  switch (type_) {
    case DIRECT:
      // Direct operations are executed directly, and take an argument.
      return makeBytes(direct_, operand_);
    case INDIRECT:
      // Indirect operations act as the argument to the OPR instruction.
      return makeBytes(OPR, static_cast<int32_t>(indirect_));
    case UNIT:
      // (Floating point) unit instructions are loaded into the register stack
      // before being invoked by the indirect instruction FPENTRY.
      return makeBytes(LDC, static_cast<int32_t>(unit_)) +
             makeBytes(OPR, static_cast<int32_t>(FPENTRY));
    default:
      throw logic_error("Invalid operation type.");
  }
}

// Instruction pattern.
const char pattern_string[] =
  //     12          2 1
  R"(^\s*(([A-Z0-9_]+):)?)"                    // Line label (if any)
    //    34         45   6
    R"(\s*(([a-z0-9]+)(\s+()"                  // Mnemonic, optional:
      R"(-?0x[0-9a-fA-F]+|)"                   // Hexadecimal argument
      R"(-?[1-9][0-9]*|)"                      // Decimal argument
      R"(-?[0-7]+|)"                           // Octal argument
      // 7          78       9          98
      R"(([A-Z0-9_]+)(\s*-\s*([A-Z0-9_]+))?)"  // Symbolic argument
    // 65 3
    R"())?)?)"                                 // end of optional
  //    A   A
  R"(\s*(#.*)?$)";                             // Comment

// Defined labels.
typedef map<string, int> Environment;

Reference::Reference(int line_number, int operation, string symbol)
    : line_number_(line_number), operation_(operation), symbol1_(symbol),
      has_symbol2_(false) {}

Reference::Reference(
    int line_number, int operation, string symbol1, string symbol2)
    : line_number_(line_number), operation_(operation), symbol1_(symbol1),
      has_symbol2_(true), symbol2_(symbol2) {}

// Apply the reference.
bool Reference::apply(vector<Operation>& operations, const Environment& env,
                      bool variable_length) const {
  int here = operations[operation_].location() + 1;

  // Look up the value of first symbol, or fail if it is undefined.
  if (env.count(symbol1_) == 0) {
    cerr << "Error: Use of undefined label " << symbol1_ << " at line "
         << line_number_ << ".\n";
    return false;
  }

  // Compute the offset using this first symbol.
  int label1 = env.at(symbol1_);
  int offset1 = label1 - here;
 
  int label2 = 0;
  int offset2 = 0;
  if (has_symbol2_) {
    // Look up the value of the second symbol, or fail if it is undefined.
    if (env.count(symbol2_) == 0) {
      cerr << "Error: Use of undefined label " << symbol2_ << " at line "
           << line_number_ << ".\n";
      return false;
    }
    // Compute the offset of the second symbol, and thus the difference.
    label2 = env.at(symbol2_);
    offset2 = label2 - here;
  }

  // The jump offset. If using variable length encoding, this is the offset from
  // the end of the first byte of the jump instruction, not necessarily the true
  // offset.
  int jump = offset1 - offset2;

  // Store the value into the operand of the associated instruction.
  operations[operation_].setOperand(jump);
  
  if (variable_length && !has_symbol2_) {
    // The adjusted jump value is the corrected jump offset, when the length of
    // the jump instruction is taken into consideration.
    int size, new_size = 1;
    // Repeatedly compute the jump offset until a fixed point is found. This
    // will happen quickly, since the size grows with the log of the offset, and
    // the offset changes only by this amount in each iteration, so it will
    // stabilise quickly.
    do {
      size = new_size;
      operations[operation_].setOperand(jump);
      new_size = operations[operation_].toBytes().length();
      here = operations[operation_].location() + new_size;
      jump = label1 - here;
    } while (new_size != size);
  }

  return true;
}

bool parseOperations(istream& input, vector<Operation>* operations,
                     Environment* labels, vector<Reference>* references) {
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
      if (labels->count(label) > 0) {
        // Duplicate label.
        cerr << "Error: Label " << label << " is redefined at line "
             << line_number << ".\n";
        return false;
      }

      (*labels)[label] = operations->size();
    }

    // Handle the operation, if there is one.
    if (mnemonic.length() > 0) {
      // Put the operation into the list.
      int op_index = operations->size();
      Operation op(op_index);

      if (argument.length() > 0) {
        int operand;

        // Evaluate the operand, if possible.
        if (symbol1.length() > 0) {
          if (symbol2.length() > 0) {
            // Symbolic expression: FOO - BAR
            references->push_back(
                Reference(line_number, op_index, symbol1, symbol2));
          } else {
            // Symbolic argument: FOO
            references->push_back(
                Reference(line_number, op_index, symbol1));
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

      operations->push_back(op);
    }
  }

  // Go through and apply all references and apply them.
  for (Reference& reference : *references)
    if (!reference.apply(*operations, *labels)) return 1;

  return true;
}

bool encodeOperations(vector<Operation> operations, Environment labels,
                      const vector<Reference>& references, ostream* output) {
  // Compute the inverse of the label map. This will be used to update the label
  // values with their new locations.
  multimap<int, string> addresses;
  for (auto label : labels)
    addresses.emplace(label.second, label.first);

  // Compute the initial selection of fragments.
  vector<string> fragments;
  bool changed = false;

  const int n = operations.size();

  // Loop until a fixed point is reached. This is guaranteed to terminate
  // because label values may only increase, and distances between labels may
  // also only increase, but neither increases unless necessary.
  do {
    // Re-assign the references with variable-length awareness.
    for (const Reference& reference : references) {
      // Update the reference. This will only return false if the environment
      // does not define one of the required labels.
      if (!reference.apply(operations, labels, true)) return false;
    }

    // Compute the code fragments.
    fragments.clear();
    int location = 0;
    changed = false;
    for (int i = 0; i < n; i++) {
      // Update the operation location.
      operations[i].set_location(location);

      // Update any labels at this location.
      auto label_range = addresses.equal_range(i);
      for (auto label = label_range.first; label != label_range.second;
           label++) {
        if (labels[label->second] != location) {
          changed = true;
          labels[label->second] = location;
        }
      }

      // Compute the instruction fragment.
      string fragment = operations[i].toBytes();
      location += fragment.length();
      fragments.push_back(fragment);
    }
  } while (changed);

  // Produce the final byte stream.
  for (const string& fragment : fragments) (*output) << fragment;
  return true;
}
