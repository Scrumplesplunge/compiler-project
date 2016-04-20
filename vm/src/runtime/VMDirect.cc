#include "VM.h"

using namespace std;

// Add constant.
DEFINE_DIRECT(ADC) {
  A += Oreg;
  // TODO: Check for overflow.
  Oreg = 0;
}

// Adjust workspace.
DEFINE_DIRECT(AJW) {
  Wptr += 4 * Oreg;
  Oreg = 0;
}

// Call subroutine.
DEFINE_DIRECT(CALL) {
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
DEFINE_DIRECT(CJ) {
  if (A == 0) {
    Iptr += Oreg;
  } else {
    pop();
  }
  Oreg = 0;
}

// Equals constant.
DEFINE_DIRECT(EQC) {
  A = (A == Oreg ? True : False);
  Oreg = 0;
}

// Jump.
DEFINE_DIRECT(J) {
  Iptr += Oreg;
  Oreg = 0;
  yield();
}

// Load constant.
DEFINE_DIRECT(LDC) {
  push(Oreg);
  Oreg = 0;
}

// Load local.
DEFINE_DIRECT(LDL) {
  push(read(Wptr + 4 * Oreg));
  Oreg = 0;
}

// Load local pointer.
DEFINE_DIRECT(LDLP) {
  push(Wptr + 4 * Oreg);
  Oreg = 0;
}

// Load non-local.
DEFINE_DIRECT(LDNL) {
  A = read(A + 4 * Oreg);
  Oreg = 0;
}

// Load non-local pointer.
DEFINE_DIRECT(LDNLP) {
  A += 4 * Oreg;
  Oreg = 0;
}

// Negative prefix.
DEFINE_DIRECT(NFIX) {
  Oreg = (~Oreg) << 4;
}

// Operate.
DEFINE_DIRECT(OPR) {
  performIndirect(static_cast<Indirect>(Oreg));
  Oreg = 0;
}

// Prefix.
DEFINE_DIRECT(PFIX) {
  Oreg <<= 4;
}

// Store local.
DEFINE_DIRECT(STL) {
  write(Wptr + 4 * Oreg, pop());
  Oreg = 0;
}

// Store non-local.
DEFINE_DIRECT(STNL) {
  write(A + 4 * Oreg, B);
  A = C;
  Oreg = 0;
}
