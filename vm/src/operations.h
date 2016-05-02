#pragma once

#include "Direct.h"
#include "Indirect.h"
#include "Unit.h"

#include <map>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <vector>

class Operation {
 public:
  enum Type {
    DIRECT,    // Direct instructions do not need OPR.
    INDIRECT,  // Indirect instructions are executed via OPR.
    UNIT,      // Unit instructions are executed via FPENTRY.
  };

  // Default operation is a NO-OP.
  Operation(int location);
  Operation(int location, Direct op, int32_t argument);
  Operation(int location, Indirect op);
  Operation(int location, Unit op);

  // Parse an argument-free operation.
  static bool fromString(std::string operation, Operation* output);

  // Parse an operation with a numeric argument.
  static bool fromString(
      std::string operation, int32_t argument, Operation* output);

  // Parse a direct operation from a byte.
  static bool fromByte(uint8_t byte, Operation* output);

  // Change the argument of the operation. Returns false if this operation does
  // not have an argument.
  void setOperand(int32_t argument) {
    if (type_ != DIRECT)
      throw std::logic_error("Cannot assign operand on non-direct operation.");
    operand_ = argument;
  }

  Type type() const { return type_; }
  
  // Get components of a direct instruction, or return false if not direct.
  bool getDirect(Direct* op, int32_t* argument) const {
    if (type_ != DIRECT) return false;
    *op = direct_;
    *argument = operand_;
    return true;
  }

  // Get the value of an indirect instruction, or return false if not indirect.
  bool getIndirect(Indirect* op) const {
    if (type_ != INDIRECT) return false;
    *op = indirect_;
    return true;
  }

  // Get the value of a unit instruction, or return false if not unit.
  bool getUnit(Unit* op) const {
    if (type_ != UNIT) return false;
    *op = unit_;
    return true;
  }

  // Output the instruction in human-readable form.
  std::string toString() const;

  // Output the instruction in byte form.
  std::string toBytes() const;

  int location() const { return location_; }
  void set_location(int value) { location_ = value; }

  void set_forced_length(int length) { forced_length_ = length; }

 private:
  Type type_;
  int location_;

  // If non-zero, instruction is forcefully encoded using auxiliary PFIX bytes
  // to extend it. If the forced length is shorter than the minimum number of
  // bytes required, an exception will be thrown.
  int forced_length_ = 0;

  union {
    struct {              // type_ == DIRECT
      Direct direct_;
      int32_t operand_;
    };
    Indirect indirect_;   // type_ == INDIRECT
    Unit unit_;           // type_ == UNIT
  };
};

// Defined labels.
typedef std::map<std::string, int> Environment;

class Reference {
 public:
  Reference(int line_number, int operation, std::string symbol);
  Reference(
      int line_number, int operation, std::string symbol1, std::string symbol2);

  // Apply the reference.
  bool apply(std::vector<Operation>& operations, const Environment& env,
             bool variable_length = false) const;

 private:
  int line_number_;

  int operation_;
  std::string symbol1_;

  bool has_symbol2_ = false;
  std::string symbol2_;
};

// Parse an assembler file into a sequence of operations.
bool parseOperations(std::istream& input, std::vector<Operation>* operations,
                     Environment* labels, std::vector<Reference>* references);

// Convert a sequence of operations into the byte-code of the corresponding
// direct instructions. This requires both an environment instance and a vector
// of references so that label addresses can be recomputed.
bool encodeOperations(std::vector<Operation> operations, Environment labels,
                      const std::vector<Reference>& references,
                      std::ostream* output);

#define DECLARE_DIRECT(name)   void perform_##name()
#define DECLARE_INDIRECT(name) void perform_##name()
#define DECLARE_UNIT(name)     void perform_##name()

#define DEFINE_DIRECT(name)   void VM::perform_##name()
#define DEFINE_INDIRECT(name) void VM::perform_##name()
#define DEFINE_UNIT(name)     void VM::perform_##name()

#define DIRECT(name)   perform_##name()
#define INDIRECT(name) perform_##name()
#define UNIT(name)     perform_##name()

#define UNIMPLEMENTED(description) {                                  \
  throw std::runtime_error(                                           \
      "Unimplemented operation: " + std::string(description));        \
}
#define UNIMPLEMENTED_FP {  \
  throw std::runtime_error(  \
      "No floating point support is provided.");  \
}
