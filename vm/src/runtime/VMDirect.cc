#include "VM.h"

using namespace std;

// Add constant.
void VM::direct_ADC() {
  A += Oreg;
  // TODO: Check for overflow.
  Oreg = 0;
}

// Adjust workspace.
void VM::direct_AJW() {
  Wptr += 4 * Oreg;
  Oreg = 0;
}

// Call subroutine.
void VM::direct_CALL() {
  Wptr -= 16;
  write(Wptr + 0, Iptr);
  write(Wptr + 4, A);
  write(Wptr + 8, B);
  write(Wptr + 12, C);
  A = Iptr;
  Iptr += Oreg;
  Oreg = 0;
}

// Conditional jump.
void VM::direct_CJ() {
  if (A == 0) {
    Iptr += Oreg;
  } else {
    A = B;
    B = C;
  }
  Oreg = 0;
}

// Equals constant.
void VM::direct_EQC() {
  A = (A == Oreg);
  Oreg = 0;
}

// Jump.
void VM::direct_J() {
  Iptr += Oreg;
  Oreg = 0;

  unique_lock<mutex> lock(queue_mu_);
  yield(lock);
}

// Load constant.
void VM::direct_LDC() {
  C = B;
  B = A;
  A = Oreg;
  Oreg = 0;
}

// Load local.
void VM::direct_LDL() {
  C = B;
  B = A;
  A = read(Wptr + 4 * Oreg);
  Oreg = 0;
}

// Load local pointer.
void VM::direct_LDLP() {
  C = B;
  B = A;
  A = Wptr + 4 * Oreg;
  Oreg = 0;
}

// Load non-local.
void VM::direct_LDNL() {
  A = read(A + 4 * Oreg);
  Oreg = 0;
}

// Load non-local pointer.
void VM::direct_LDNLP() {
  A += 4 * Oreg;
  Oreg = 0;
}

// Negative prefix.
void VM::direct_NFIX() {
  Oreg = (~Oreg) << 4;
}

// Operate.
void VM::direct_OPR() {
  performIndirect(static_cast<Indirect>(Oreg));
  Oreg = 0;
}

// Prefix.
void VM::direct_PFIX() {
  Oreg <<= 4;
}

// Store local.
void VM::direct_STL() {
  write(Wptr + 4 * Oreg, A);
  A = B;
  B = C;
  Oreg = 0;
}

// Store non-local.
void VM::direct_STNL() {
  write(A + 4 * Oreg, B);
  A = C;
  Oreg = 0;
}
