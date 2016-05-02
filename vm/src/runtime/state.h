#pragma once

#include <memory>
#include <stdint.h>

struct State {
  int32_t Wptr;     // Workspace pointer.
  int32_t Iptr;     // Instruction pointer.
  int32_t A, B, C;  // Register stack.
  int32_t Oreg;     // Operand register.

  int32_t BptrReg;   // Process queue back pointer.
  int32_t FptrReg;   // Process queue front pointer.

  // RAM.
  int32_t* memory;
  int32_t memory_size;
};
