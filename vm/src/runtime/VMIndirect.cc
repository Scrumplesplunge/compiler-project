#include "VM.h"

#include <iostream>
#include <stdexcept>
#include <stdio.h>

using namespace std;

// Add.
void VM::indirect_ADD() {
  A += B;
  B = C;
  // TODO: Check for overflow.
}

// Bitwise and.
void VM::indirect_AND() {
  A = A & B;
  B = C;
}

// Difference.
void VM::indirect_DIFF() {
  A = B - A;
  B = C;
}

// Disable skip.
void VM::indirect_DISS() {
  if (B && read(Wptr) == NoneSelected) {
    write(Wptr, A);
    A = True;
  } else {
    A = False;
    B = C;
  }
}

// Divide.
void VM::indirect_DIV() {
  if (A == 0 || (A == -1 && B == MostNeg)) {
    // TODO: Handle error.
  } else {
    A = B / A;
    B = C;
  }
}

// Duplicate.
void VM::indirect_DUP() {
  C = B;
  B = A;
}

// Enable skip.
void VM::indirect_ENBS() {
  if (A) write(Wptr - 12, Ready);
}

// End process.
void VM::indirect_ENDP() {
  if (read(A + 4) == 1) {
    // Continue as process with waiting workspace A.
    Wptr = A;
    Iptr = read(A);
  } else {
    // Release control.
    write(A + 4, read(A + 4) - 1);
   
    #ifdef DISABLE_MUTEX
      stop(nullptr);
    #else
      unique_lock<mutex> lock(queue_mu_);
      stop(lock);
    #endif
  }
}

// Greater than.
void VM::indirect_GT() {
  A = (B > A);
  B = C;
}

// Load byte.
void VM::indirect_LB() {
  A = readByte(A);
}

// Load pointer to instruction.
void VM::indirect_LDPI() {
  A = A + Iptr;
}

// Loop end.
void VM::indirect_LEND() {
  if (read(B + 4) > 1) {
    write(B, read(B) + 1);  // Increment control variable.
    write(B + 4, read(B + 4) - 1);  // Decrement iteration count.
    Iptr -= A;
  }

  #ifdef DISABLE_MUTEX
    yield(nullptr);
  #else
    unique_lock<mutex> lock(queue_mu_);
    yield(lock);
  #endif
}

// Minimum integer.
void VM::indirect_MINT() {
  C = B;
  B = A;
  A = MostNeg;
}

// Multiply.
void VM::indirect_MUL() {
  // TODO: Check for overflow.
  A = A * B;
  B = C;
}

// Bitwise not.
void VM::indirect_NOT() {
  A = ~A;
}

// Bitwise or.
void VM::indirect_OR() {
  A = A | B;
  B = C;
}

// Output word. This is not treated as a channel operation because it is just a
// thin wrapper around OUT.
void VM::indirect_OUTWORD() {
  write(Wptr, A);
  A = 4;
  C = Wptr;

  indirect_OUT();
}

// Remainder.
void VM::indirect_REM() {
  if (A == 0 || ((A == -1 && B == MostNeg))) {
    // TODO: Handle error.
  } else {
    A = B % A;
    B = C;
  }
}

// Return from subroutine.
void VM::indirect_RET() {
  Iptr = read(Wptr + 4);
  A = read(Wptr + 8);
  B = read(Wptr + 12);
  C = read(Wptr + 16);
  Wptr += 16;
}

// Reverse.
void VM::indirect_REV() {
  A ^= B;
  B ^= A;
  A ^= B;
}

// Run process.
void VM::indirect_RUNP() {
  Wptr = (A & ~0x3);
  Iptr = read(Wptr - 4);
}

// Store byte.
void VM::indirect_SB() {
  writeByte(A, static_cast<int8_t>(B));
  A = C;
}

// Shift left.
void VM::indirect_SHL() {
  A = B << A;
  B = C;
}

// Shift right.
void VM::indirect_SHR() {
  A = static_cast<int32_t>(static_cast<uint32_t>(B) >> A);
  B = C;
}

// Start process.
void VM::indirect_STARTP() {
  #ifdef DISABLE_MUTEX
    schedule(nullptr);
  #else
    unique_lock<mutex> lock(queue_mu_);
    schedule(lock);
  #endif

  Wptr = A;
  Iptr += B;
}

// Stop process.
void VM::indirect_STOPP() {
  #ifdef DISABLE_MUTEX
    stop(nullptr);
  #else
    unique_lock<mutex> lock(queue_mu_);
    stop(lock);
  #endif
}

// Subtract.
void VM::indirect_SUB() {
  // TODO: Check for overflow.
  A = B - A;
  B = C;
}

// Word subscript.
void VM::indirect_WSUB() {
  A += B * 4;
  B = C;
}

// Exclusive or.
void VM::indirect_XOR() {
  A = A ^ B;
  B = C;
}

///////////////////////////////////////////////////////////////////////////////
// META OPERATIONS BEGIN                                                     //
///////////////////////////////////////////////////////////////////////////////

// Print a byte to the console.
void VM::indirect_PUTC() {
  char c = static_cast<char>(A);
  cout << c << flush;
  A = B;
  B = C;
}

// Print A bytes pointed to by B to the console.
void VM::indirect_PUTS() {
  for (int i = 0; i < A; i++) {
    char c = static_cast<char>(readByte(B + i));
    cout << c << flush;
  }
}

// Print a word to the console (decimal).
void VM::indirect_PRINTDEC() {
  cout << A << flush;
  A = B;
  B = C;
}

// Print a word to the console (hexadecimal).
void VM::indirect_PRINTHEX() {
  printf("%x", static_cast<uint32_t>(A));
  A = B;
  B = C;
}

// Print A words from the array pointed to by B.
void VM::indirect_PRINTR() {
  for (int i = 0; i < A; i++)
    printf("[0x%08x] = %d\n", B + 4 * i, read(B + 4 * i));
  A = C;
}

// VIRTUAL OPERATIONS.

// Alt start.
void VM::indirect_ALT() {
  write(Wptr - 12, Enabling);
}

// Alt end.
void VM::indirect_ALTEND() {
  Iptr += read(Wptr);
}

// Alt wait.
void VM::indirect_ALTWT() {
  write(Wptr, NoneSelected);  // Indicate that no branch yet selected.
  if (read(Wptr - 12) != Ready) {
    #ifdef DISABLE_MUTEX
      deschedule(nullptr);
    #else
      unique_lock<mutex> lock(queue_mu_);
      deschedule(lock);
    #endif
  }
}

// Disable channel.
void VM::indirect_DISC() {
  // Skip processing if the guard condition is false.
  if (!B) {
    A = False;
    return;
  }

  bool is_ready = (read(C) != makeWdesc(Wptr));

  // A channel guard is detectably ready if the value stored in the channel does
  // not match the current workspace descriptor.
  if (is_ready) {
    if (read(Wptr) == NoneSelected) {
      // Channel is the first ready guard.
      write(Wptr, A);
      A = True;
    } else {
      // Another guard was ready before this one.
      A = False;
    }
  } else {
    // The channel is not ready. Restore it to its default value.
    write(C, NotProcess);
    A = False;
  }
}

// Enable channel.
void VM::indirect_ENBC() {
  // Skip processing if the guard is false.
  if (!A) return;

  bool is_ready = (read(B) != makeWdesc(Wptr));

  if (is_ready) {
    // Another process is waiting on channel B.
    write(Wptr - 12, Ready);
  } else {
    // No process waiting on channel B.
    write(B, makeWdesc(Wptr));
  }
}

// Input message.
void VM::indirect_IN() {
  int32_t chan_value = read(B);
  if (chan_value == NotProcess) {
    // No process is currently waiting on this channel. Initiate
    // communication by putting this process id in the channel.
    write(B, makeWdesc(Wptr));
    write(Wptr - 12, C);

    #ifdef DISABLE_MUTEX
      deschedule(nullptr);
    #else
      unique_lock<mutex> lock(queue_mu_);
      deschedule(lock);
    #endif
  } else {
    // A process is waiting. The communication can proceed.
    int32_t source = read((chan_value & ~0x3) - 12);

    for (int i = 0; i < A; i++)
      writeByte(C + i, readByte(source + i));

    // Reschedule the other thread.
    #ifdef DISABLE_MUTEX
      schedule(chan_value, nullptr);
    #else
      {
        unique_lock<mutex> lock(queue_mu_);
        schedule(chan_value, lock);
      }
    #endif

    // Reset the channel.
    write(B, NotProcess);
  }
}

// Output message.
void VM::indirect_OUT() {
  int32_t chan_value = read(B);
  if (chan_value == NotProcess) {
    // No process is currently waiting on this channel. Initiate
    // communication by putting this process id in the channel.
    write(B, makeWdesc(Wptr));
    write(Wptr - 12, C);

    #ifdef DISABLE_MUTEX
      deschedule(nullptr);
    #else
      unique_lock<mutex> lock(queue_mu_);
      deschedule(lock);
    #endif
  } else {
    int32_t dest_address = (chan_value & ~0x3) - 12;
    int32_t dest = read(dest_address);

    if (dest == Waiting) {
      // A process is waiting via an ALT. Initiate the communication and
      // reschedule the process.
      write(B, makeWdesc(Wptr));
      write(dest_address, Ready);
      write(Wptr - 12, C);

      #ifdef DISABLE_MUTEX
        schedule(chan_value, nullptr);
        deschedule(nullptr);
      #else
        unique_lock<mutex> lock(queue_mu_);
        schedule(chan_value, lock);
        deschedule(lock);
      #endif
    } else {
      // A process is waiting directly. The communication can proceed.
      for (int i = 0; i < A; i++)
        writeByte(dest + i, readByte(C + i));
      // Reschedule the other thread.
      #ifdef DISABLE_MUTEX
        schedule(chan_value, nullptr);
      #else
        {
          unique_lock<mutex> lock(queue_mu_);
          schedule(chan_value, lock);
        }
      #endif

      // Reset the channel.
      write(B, NotProcess);
    }
  }
}

// Reset channel.
void VM::indirect_RESETCH() {
  write(A, NotProcess);
}
