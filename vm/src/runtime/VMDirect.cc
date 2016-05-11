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
    A = B;
    B = C;
  }
  Oreg = 0;
}

// Equals constant.
DEFINE_DIRECT(EQC) {
  A = (A == Oreg);
  Oreg = 0;
}

// Jump.
DEFINE_DIRECT(J) {
  Iptr += Oreg;
  Oreg = 0;

  unique_lock<mutex> lock(queue_mu_);
  yield(lock);
}

// Load constant.
DEFINE_DIRECT(LDC) {
  C = B;
  B = A;
  A = Oreg;
  Oreg = 0;
}

// Load local.
DEFINE_DIRECT(LDL) {
  C = B;
  B = A;
  A = read(Wptr + 4 * Oreg);
  Oreg = 0;
}

// Load local pointer.
DEFINE_DIRECT(LDLP) {
  C = B;
  B = A;
  A = Wptr + 4 * Oreg;
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
  write(Wptr + 4 * Oreg, A);
  A = B;
  B = C;
  Oreg = 0;
}

// Store non-local.
DEFINE_DIRECT(STNL) {
  write(A + 4 * Oreg, B);
  A = C;
  Oreg = 0;
}
