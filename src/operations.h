#pragma once

#include "Direct.h"
#include "Indirect.h"
#include "Unit.h"

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
  Operation();

  Operation(Direct op, int32_t argument);
  Operation(Indirect op);
  Operation(Unit op);

  // Parse an argument-free operation.
  static bool fromString(std::string operation, Operation* output);

  // Parse an operation with a numeric argument.
  static bool fromString(
      std::string operation, int32_t argument, Operation* output);

  // Change the argument of the operation. Returns false if this operation does
  // not have an argument.
  bool setOperand(int32_t argument) {
    if (type_ != DIRECT) return false;
    operand_ = argument;
    return true;
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

 private:
  Type type_;
  union {
    struct {              // type_ == DIRECT
      Direct direct_;
      int32_t operand_;
    };
    Indirect indirect_;   // type_ == INDIRECT
    Unit unit_;           // type_ == UNIT
  };
};

// Parse an assembler file.
bool parseOperations(std::istream& input, std::vector<Operation>* output);

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
