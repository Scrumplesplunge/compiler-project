#include "State.h"

#include <iostream>
#include <stdexcept>
#include <stdio.h>

using namespace std;

// Add.
DEFINE_INDIRECT(ADD) {
  push(pop() + pop());
  // TODO: Check for overflow.
}

// Alt start.
DEFINE_INDIRECT(ALT) {
  write(Wptr - 12, Enabling);
}

// Alt end.
DEFINE_INDIRECT(ALTEND) {
  Iptr += read(Wptr);
}

// Alt wait.
DEFINE_INDIRECT(ALTWT) {
  write(Wptr, -1);  // Indicate that no branch yet selected.
  if (read(Wptr - 12) != Ready) deschedule();
}

// Bitwise and.
DEFINE_INDIRECT(AND) {
  push(pop() & pop());
}

// Byte count.
DEFINE_INDIRECT(BCNT) {
  A *= 4;
}

// Count bits set in word.
DEFINE_INDIRECT(BITCNT) {
  UNIMPLEMENTED("BITCNT - Count bits set in a word.");
}

// Reverse bottom N bits in word.
DEFINE_INDIRECT(BITREVNBITS) {
  UNIMPLEMENTED("BITREVNBITS - Reverse the bottom N bits in a word.");
}

// Reverse bits in word.
DEFINE_INDIRECT(BITREVWORD) {
  UNIMPLEMENTED("BITREVWORD - Reverse the bits in a word.");
}

// Byte subscript.
DEFINE_INDIRECT(BSUB) {
  push(pop() + pop());
}

// Check count from one.
DEFINE_INDIRECT(CCNT1) {
  if (B == 0) setError();
  if (B > A) setError();
  pop();
}

// Check floating point error.
DEFINE_INDIRECT(CFLERR) {
  UNIMPLEMENTED("CFLERR - Check if a floating point value is infinity or NaN.");
}

// Clear the halt-on-error flag.
DEFINE_INDIRECT(CLRHALTERR) {
  HaltOnError = false;
}

// Calculate CRC on byte.
DEFINE_INDIRECT(CRCBYTE) {
  UNIMPLEMENTED("CRCBYTE - Calculate CRC on byte.");
}

// Calculate CRC on word.
DEFINE_INDIRECT(CRCWORD) {
  UNIMPLEMENTED("CRCWORD - Calculate CRC on word.");
}

// Check single.
DEFINE_INDIRECT(CSNGL) {
  UNIMPLEMENTED("CSNGL - Check if the long value in A and B fits in a word.");
}

// Check subscript from zero.
DEFINE_INDIRECT(CSUB0) {
  if (B >= A) setError();
  pop();
}

// Check word.
DEFINE_INDIRECT(CWORD) {
  if (B >= A || B < -A) setError();
  pop();
}

// Difference.
DEFINE_INDIRECT(DIFF) {
  A = B - A;
  B = C;
}

// Disable channel.
DEFINE_INDIRECT(DISC) {
  UNIMPLEMENTED("DISC - Disable channel.");
  // TODO: Fix this implementation.
  // if (B && read(Wptr) == NoneSelected) {
  //   write(Wptr, A);
  //   A = True;
  // } else {
  //   A = False;
  // }
}

// Disable skip.
DEFINE_INDIRECT(DISS) {
  if (B && read(Wptr) == NoneSelected) {
    write(Wptr, A);
    A = True;
  } else {
    A = False;
    B = C;
  }
}

// Disable timer.
DEFINE_INDIRECT(DIST) {
  if (B && read(Wptr) == NoneSelected && after(time(), C)) {
    write(Wptr, A);
    A = True;
  } else {
    A = False;
  }
}

// Divide.
DEFINE_INDIRECT(DIV) {
  if (A == 0 || (A == -1 && B == MostNeg)) {
    setError();
  } else {
    A = B / A;
    B = C;
  }
}

// Duplicate.
DEFINE_INDIRECT(DUP) {
  push(A);
}

// Enable channel.
DEFINE_INDIRECT(ENBC) {
  if (A) {
    if (read(B) == NotProcess) {
      // No process waiting on channel B.
      write(B, Wdesc());
    } else if (read(B) != Wdesc()) {
      // Another process is waiting on channel B.
      write(Wptr - 12, Ready);
      B = C;
    }
  }
}

// Enable skip.
DEFINE_INDIRECT(ENBS) {
  if (A) write(Wptr - 12, Ready);
}

// Enable timer.
DEFINE_INDIRECT(ENBT) {
  if (A) {
    if (read(Wptr - 16) == TimeNotSet) {
      // Time not set: set the time.
      write(Wptr - 16, TimeSet);
      write(Wptr - 20, B);
    } else if (after(read(Wptr - 20), B)) {
      // Time set, but the new one is sooner.
      write(Wptr - 20, B);
    }
  }
  B = C;
}

// End process.
DEFINE_INDIRECT(ENDP) {
  if (read(A + 4) == 1) {
    // Continue as process with waiting workspace A.
    Wptr = A;
    Iptr = read(A);
  } else {
    // Release control.
    write(A + 4, read(A + 4) - 1);
    stop();
  }
}

DEFINE_INDIRECT(FMUL) UNIMPLEMENTED_FP;         // Fractional multiply.
DEFINE_INDIRECT(FPADD) UNIMPLEMENTED_FP;        // Add.
DEFINE_INDIRECT(FPB32TOR64) UNIMPLEMENTED_FP;   // Bit 32 to real 64.
DEFINE_INDIRECT(FPCHKERR) UNIMPLEMENTED_FP;     // Check floating point error.
DEFINE_INDIRECT(FPDIV) UNIMPLEMENTED_FP;        // Divide.
DEFINE_INDIRECT(FPDUP) UNIMPLEMENTED_FP;        // Duplicate.
DEFINE_INDIRECT(FPENTRY) UNIMPLEMENTED_FP;      // Unit entry.
DEFINE_INDIRECT(FPEQ) UNIMPLEMENTED_FP;         // Equals.
DEFINE_INDIRECT(FPGT) UNIMPLEMENTED_FP;         // Greater than.
DEFINE_INDIRECT(FPI32TOR32) UNIMPLEMENTED_FP;   // Int 32 to real 32.
DEFINE_INDIRECT(FPI32TOR64) UNIMPLEMENTED_FP;   // Int 32 to real 64.
DEFINE_INDIRECT(FPINT) UNIMPLEMENTED_FP;        // Round to floating integer.
DEFINE_INDIRECT(FPLDNLADDDB) UNIMPLEMENTED_FP;  // Load non-local and add double.
DEFINE_INDIRECT(FPLDNLADDSN) UNIMPLEMENTED_FP;  // Load non-local and add single.
DEFINE_INDIRECT(FPLDNLDB) UNIMPLEMENTED_FP;     // Load non-local double.
DEFINE_INDIRECT(FPLDNLDBI) UNIMPLEMENTED_FP;    // Load non-local indexed double.
DEFINE_INDIRECT(FPLDNLMULDB) UNIMPLEMENTED_FP;  // Load non-local and multiply double.
DEFINE_INDIRECT(FPLDNLMULSN) UNIMPLEMENTED_FP;  // Load non-local and multiply single.
DEFINE_INDIRECT(FPLDNLSN) UNIMPLEMENTED_FP;     // Load non-local single.
DEFINE_INDIRECT(FPLDNLSNI) UNIMPLEMENTED_FP;    // Load non-local indexed single.
DEFINE_INDIRECT(FPLDZERODB) UNIMPLEMENTED_FP;   // Load zero double.
DEFINE_INDIRECT(FPLDZEROSN) UNIMPLEMENTED_FP;   // Load zero single.
DEFINE_INDIRECT(FPMUL) UNIMPLEMENTED_FP;        // Multiply.
DEFINE_INDIRECT(FPNAN) UNIMPLEMENTED_FP;        // Not-a-number test.
DEFINE_INDIRECT(FPNOTFINITE) UNIMPLEMENTED_FP;  // Not finite test.
DEFINE_INDIRECT(FPORDERED) UNIMPLEMENTED_FP;    // Orderability.
DEFINE_INDIRECT(FPREMFIRST) UNIMPLEMENTED_FP;   // Remainder first step.
DEFINE_INDIRECT(FPREMSTEP) UNIMPLEMENTED_FP;    // Remainder iteration.
DEFINE_INDIRECT(FPREV) UNIMPLEMENTED_FP;        // Reverse.
DEFINE_INDIRECT(FPRTOI32) UNIMPLEMENTED_FP;     // Real to int 32.
DEFINE_INDIRECT(FPSTNLDB) UNIMPLEMENTED_FP;     // Store non-local double.
DEFINE_INDIRECT(FPSTNLI32) UNIMPLEMENTED_FP;    // Store non-local integer.
DEFINE_INDIRECT(FPSTNLSN) UNIMPLEMENTED_FP;     // Store non-local single.
DEFINE_INDIRECT(FPSUB) UNIMPLEMENTED_FP;        // Subtract.
DEFINE_INDIRECT(FPTESTERR) UNIMPLEMENTED_FP;    // Test floating point flag false.

// Generalised adjust workspace.
DEFINE_INDIRECT(GAJW) {
  // Mask.
  A = A & ~0x3;

  // Swap.
  Wptr ^= A;
  A ^= Wptr;
  Wptr ^= A;
}

// Generalised call.
DEFINE_INDIRECT(GCALL) {
  // Swap.
  Iptr ^= A;
  A ^= Iptr;
  Iptr ^= A;
}

// Greater than.
DEFINE_INDIRECT(GT) {
  A = (B > A ? True : False);
  B = C;
}

// Input message.
DEFINE_INDIRECT(IN) {
  if (isExternalChannel(B)) {
    // TODO: Need to implement external channel communication.
    throw runtime_error("External communication is not implemented.");
  } else {
    int32_t chan_value = read(B);
    if (chan_value == NotProcess) {
      // No process is currently waiting on this channel. Initiate
      // communication by putting this process id in the channel.
      write(B, Wdesc());
      write(Wptr - 12, C);
      deschedule();
    } else {
      // A process is waiting. The communication can proceed.
      int32_t source = read((chan_value & ~0x3) - 12);

      // TODO: Special behaviour is required to handle alt constructs.
      if (source == Enabling || source == Waiting || source == Ready)
        throw runtime_error("Channel alternatives not yet implemented.");

      for (int i = 0; i < A; i++)
        writeByte(C + i, readByte(source + i));
      // Reschedule the other thread.
      schedule(chan_value);

      // Reset the channel.
      write(B, NotProcess);
    }
  }
}

// Long add.
DEFINE_INDIRECT(LADD) {
  UNIMPLEMENTED("LADD - Long addition.");
}

// Load byte.
DEFINE_INDIRECT(LB) {
  A = readByte(A);
}

// Long difference.
DEFINE_INDIRECT(LDIFF) {
  UNIMPLEMENTED("LDIFF - Long difference.");
}

DEFINE_INDIRECT(LDINF) UNIMPLEMENTED_FP;  // Load single FP infinity.

// Long divide.
DEFINE_INDIRECT(LDIV) {
  UNIMPLEMENTED("LDIV - Long divide.");
}

// Load pointer to instruction.
DEFINE_INDIRECT(LDPI) {
  A = A + Iptr;
}

// Load current priority.
DEFINE_INDIRECT(LDPRI) {
  push(priority);
}

// Load timer.
DEFINE_INDIRECT(LDTIMER) {
  push(time());
}

// Loop end.
DEFINE_INDIRECT(LEND) {
  if (read(B + 4) > 1) {
    write(B, read(B) + 1);  // Increment control variable.
    write(B + 4, read(B + 4) - 1);  // Decrement iteration count.
    Iptr -= A;
  }
  yield();
}

// Long shift left.
DEFINE_INDIRECT(LSHL) {
  UNIMPLEMENTED("LSHL - Long shift left.");
}

// Long shift right.
DEFINE_INDIRECT(LSHR) {
  UNIMPLEMENTED("LSHR - Long shift right.");
}

// Long subtract.
DEFINE_INDIRECT(LSUB) {
  UNIMPLEMENTED("LSUB - Long subtract.");
}

// Long sum.
DEFINE_INDIRECT(LSUM) {
  UNIMPLEMENTED("LSUM - Long sum.");
}

// Minimum integer.
DEFINE_INDIRECT(MINT) {
  push(MostNeg);
}

// Move message.
DEFINE_INDIRECT(MOVE) {
  for (int i = 0; i < A; i++)
    writeByte(B + i, readByte(C + i));
}

// 2D block move.
DEFINE_INDIRECT(MOVE2DALL) {
  UNIMPLEMENTED("MOVE2DALL - 2D block move.");
}

// Initialise data for 2D block move.
DEFINE_INDIRECT(MOVE2DINIT) {
  UNIMPLEMENTED("MOVE2DINIT - Initialise data for 2D block move.");
}

// 2D block move only non-zero bytes.
DEFINE_INDIRECT(MOVE2DNONZERO) {
  UNIMPLEMENTED("MOVE2DNONZERO - 2D block move only non-zero bytes.");
}

// 2D block move only zero bytes.
DEFINE_INDIRECT(MOVE2DZERO) {
  UNIMPLEMENTED("MOVE2DZERO - 2D block move only zero bytes.");
}

// Multiply.
DEFINE_INDIRECT(MUL) {
  // TODO: Check for overflow.
  A = A * B;
  B = C;
}

DEFINE_INDIRECT(NORM) UNIMPLEMENTED_FP;  // Normalise floating point value.

// Bitwise not.
DEFINE_INDIRECT(NOT) {
  A = ~A;
}

// Bitwise or.
DEFINE_INDIRECT(OR) {
  A = A | B;
  B = C;
}

// Output message.
DEFINE_INDIRECT(OUT) {
  if (isExternalChannel(B)) {
    // TODO: Need to implement external channel communication.
    throw runtime_error("External communication is not implemented.");
  } else {
    int32_t chan_value = read(B);
    if (chan_value == NotProcess) {
      // No process is currently waiting on this channel. Initiate
      // communication by putting this process id in the channel.
      write(B, Wdesc());
      write(Wptr - 12, C);
      deschedule();
    } else {
      // A process is waiting. The communication can proceed.
      int32_t dest = read((chan_value & ~0x3) - 12);

      // TODO: Special behaviour is required to handle alt constructs.
      if (dest == Enabling || dest == Waiting || dest == Ready)
        throw runtime_error("Channel alternatives not yet implemented.");

      for (int i = 0; i < A; i++)
        writeByte(dest + i, readByte(C + i));
      // Reschedule the other thread.
      schedule(chan_value);

      // Reset the channel.
      write(B, NotProcess);
    }
  }
}

// Output byte.
DEFINE_INDIRECT(OUTBYTE) {
  UNIMPLEMENTED("OUTBYTE - Output byte.");
}

// Output word.
DEFINE_INDIRECT(OUTWORD) {
  int32_t word = pop();
  int32_t chan = pop();
  write(Wptr, word);

  push(Wptr);
  push(chan);
  push(4);

  DIRECT(OUT);
}

DEFINE_INDIRECT(POSTNORMSN) UNIMPLEMENTED_FP;  // Post-normalise correction.

// Product.
DEFINE_INDIRECT(PROD) {
  A = A * B;
  B = C;
}

// Remainder.
DEFINE_INDIRECT(REM) {
  if (A == 0 || ((A == -1 && B == MostNeg))) {
    setError();
  } else {
    A = B % A;
    B = C;
  }
}

// Reset channel.
DEFINE_INDIRECT(RESETCH) {
  if (isExternalChannel(A)) {
    // TODO: Need to implement external channel communication.
    throw runtime_error("External communication is not implemented.");
  } else {
    write(A, NotProcess);
  }
}

// Return.
DEFINE_INDIRECT(RET) {
  Iptr = read(Wptr);
  Wptr += 16;
}

// Reverse.
DEFINE_INDIRECT(REV) {
  A ^= B;
  B ^= A;
  A ^= B;
}

DEFINE_INDIRECT(ROUNDSN) UNIMPLEMENTED_FP;  // Round floating point number.

// Run process.
DEFINE_INDIRECT(RUNP) {
  if ((A & 0x3) < priority) {
    // Process is less important than the running process.
    schedule(A);
  } else {
    // Process can run.
    Wptr = (A & ~0x3);
    priority = A & 0x3;
    Iptr = read(Wptr - 4);
  }
}

// Save high priority queue registers.
DEFINE_INDIRECT(SAVEH) {
  write(A, FptrReg[0]);
  write(A + 4, BptrReg[0]);
  pop();
}

// Save low priority queue registers.
DEFINE_INDIRECT(SAVEL) {
  write(A, FptrReg[1]);
  write(A + 4, BptrReg[1]);
  pop();
}

// Store byte.
DEFINE_INDIRECT(SB) {
  writeByte(A, static_cast<int8_t>(B));
  A = C;
}

// Set error.
DEFINE_INDIRECT(SETERR) {
  setError();
}

// Set halt-on-error.
DEFINE_INDIRECT(SETHALTERR) {
  HaltOnError = true;
}

// Shift left.
DEFINE_INDIRECT(SHL) {
  A = B << A;
  B = C;
}

// Shift right.
DEFINE_INDIRECT(SHR) {
  A = static_cast<int32_t>(static_cast<uint32_t>(B) >> A);
  B = C;
}

// Start process.
DEFINE_INDIRECT(STARTP) {
  schedule();
  Wptr = A;
  Iptr += B;
}

// Store high priority back pointer.
DEFINE_INDIRECT(STHB) {
  BptrReg[0] = pop();
}

// Store high priority front pointer.
DEFINE_INDIRECT(STHF) {
  FptrReg[0] = pop();
}

// Store low priority back pointer.
DEFINE_INDIRECT(STLB) {
  BptrReg[1] = pop();
}

// Store low priority front pointer.
DEFINE_INDIRECT(STLF) {
  FptrReg[1] = pop();
}

// Stop on error.
DEFINE_INDIRECT(STOPERR) {
  if (Error) stop();
}

// Stop process.
DEFINE_INDIRECT(STOPP) {
  stop();
}

// Store timer.
DEFINE_INDIRECT(STTIMER) {
  ClockReg[0] = A;
  ClockReg[1] = A;
  pop();
}

// Subtract.
DEFINE_INDIRECT(SUB) {
  // TODO: Check for overflow.
  A = B - A;
  B = C;
}

// Sum.
DEFINE_INDIRECT(SUM) {
  A = B + A;
  B = C;
}

// Timer alt start.
DEFINE_INDIRECT(TALT) {
  UNIMPLEMENTED("TALT - Timer alt start.");
  // write(Wptr - 16) = TimeNotSet;
  // write(Wptr - 12) = Enabling;
}

// Timer alt wait.
DEFINE_INDIRECT(TALTWT) {
  UNIMPLEMENTED("TALTWT - Timer alt wait.");
  // write(Wptr, -1);  // Indicate that no branch yet selected.
  // // TODO: Check that this works.
  // if (read(Wptr - 12) != Ready || read(Wptr - 16) != TimeSet)
  // deschedule();
}
 
// Test error false and clear.
DEFINE_INDIRECT(TESTERR) {
  push(Error ? False : True);
  Error = false;
}

// Test halt-on-error flag.
DEFINE_INDIRECT(TESTHALTERR) {
  push(HaltOnError ? True : False);
}

// Test for processor analysing.
DEFINE_INDIRECT(TESTPRANAL) {
  UNIMPLEMENTED("TESTPRANAL - Test if the processor was reset to analyse.");
}

// Timer input.
DEFINE_INDIRECT(TIN) {
  UNIMPLEMENTED("TIN - Timer input.");
}

DEFINE_INDIRECT(UNPACKSN) UNIMPLEMENTED_FP;  // Unpack floating-point number.

// Word count.
DEFINE_INDIRECT(WCNT) {
  C = B;
  B = A & 0x03;
  A = static_cast<int32_t>(static_cast<uint32_t>(A) >> 2);
}

// Word subscript.
DEFINE_INDIRECT(WSUB) {
  A += B * 4;
  B = C;
}

// Form double-word subscript.
DEFINE_INDIRECT(WSUBDB) {
  UNIMPLEMENTED("WSUBDB - Form double word subscript.");
  // A += B * 8;
  // B = C;
}

// Extend to double-word.
DEFINE_INDIRECT(XDBLE) {
  UNIMPLEMENTED("XDBLE - Extend to double-word.");
}

// Exclusive or.
DEFINE_INDIRECT(XOR) {
  A = A ^ B;
  B = C;
}

// Extend to word.
DEFINE_INDIRECT(XWORD) {
  UNIMPLEMENTED("XWORD - Extend to word.");
}

///////////////////////////////////////////////////////////////////////////////
// META OPERATIONS BEGIN                                                     //
///////////////////////////////////////////////////////////////////////////////

// Print a byte to the console.
DEFINE_INDIRECT(PUTC) {
  char c = static_cast<char>(pop());
  putc(c, stdout);
}

// Print a word to the console (decimal).
DEFINE_INDIRECT(PRINTDEC) {
  printf("%d", pop());
}

// Print a word to the console (hexadecimal).
DEFINE_INDIRECT(PRINTHEX) {
  printf("%x", static_cast<uint32_t>(pop()));
}
