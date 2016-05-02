#include "VM.h"

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
  A = A & B;
  B = C;
}

// Difference.
DEFINE_INDIRECT(DIFF) {
  A = B - A;
  B = C;
}

// Disable channel.
DEFINE_INDIRECT(DISC) {
  // Skip processing if the guard condition is false.
  if (!B) {
    A = False;
    return;
  }

  bool is_ready = false;
  if (isExternalChannelReader(C)) {
    ChannelReader& chan = channelReader(C);
    chan.markDisabled();
    is_ready = chan.is_ready();
  } else {
    is_ready = (read(C) != Wdesc());
  }

  // A channel guard is detectably ready if the value stored in the channel does
  // not match the current workspace descriptor.
  if (read(Wptr) == NoneSelected && is_ready) {
    write(Wptr, A);
    A = True;
  } else {
    A = False;
  }
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
  // Skip processing if the guard is false.
  if (!A) return;

  bool is_ready = false;
  if (isExternalChannelReader(B)) {
    ChannelReader& chan = channelReader(B);
    chan.markEnabled(Wdesc());
    is_ready = chan.is_ready();
  } else {
    is_ready = (read(B) != Wdesc());
  }

  if (read(B) == NotProcess) {
    // No process waiting on channel B.
    write(B, Wdesc());
  } else if (is_ready) {
    // Another process is waiting on channel B.
    write(Wptr - 12, Ready);
    B = C;
  }
}

// Enable skip.
DEFINE_INDIRECT(ENBS) {
  if (A) write(Wptr - 12, Ready);
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

// Greater than.
DEFINE_INDIRECT(GT) {
  A = (B > A ? True : False);
  B = C;
}

// Input message.
DEFINE_INDIRECT(IN) {
  if (isExternalChannelReader(B)) {
    ChannelReader& chan = channelReader(B);
    if (chan.is_ready()) {
      // Perform the read. The channel will wake up the other end automatically.
      chan.read(C, A, this);
    } else {
      // Writer is not waiting. Wait for the writer.
      chan.readWait(Wdesc());
      deschedule();
    }
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

      for (int i = 0; i < A; i++)
        writeByte(C + i, readByte(source + i));

      // Reschedule the other thread.
      schedule(chan_value);

      // Reset the channel.
      write(B, NotProcess);
    }
  }
}

// Load byte.
DEFINE_INDIRECT(LB) {
  A = readByte(A);
}

// Load pointer to instruction.
DEFINE_INDIRECT(LDPI) {
  A = A + Iptr;
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

// Minimum integer.
DEFINE_INDIRECT(MINT) {
  push(MostNeg);
}

// Multiply.
DEFINE_INDIRECT(MUL) {
  // TODO: Check for overflow.
  A = A * B;
  B = C;
}

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
  if (isExternalChannelWriter(B)) {
    ChannelWriter& chan = channelWriter(B);
    if (chan.is_ready()) {
      // Channel operation can occur immediately. Channel will automatically
      // wake up the other end.
      chan.write(*this, C, A);
    } else {
      // Channel is not ready. Wait for the reader.
      chan.writeWait(Wdesc());
      deschedule();
    }
  } else {
    int32_t chan_value = read(B);
    if (chan_value == NotProcess) {
      // No process is currently waiting on this channel. Initiate
      // communication by putting this process id in the channel.
      write(B, Wdesc());
      write(Wptr - 12, C);
      deschedule();
    } else {
      int32_t dest_address = (chan_value & ~0x3) - 12;
      int32_t dest = read(dest_address);

      if (dest == Waiting) {
        // A process is waiting via an ALT. Initiate the communication and
        // reschedule the process.
        write(B, Wdesc());
        write(dest_address, Ready);
        write(Wptr - 12, C);
        schedule(chan_value);
        deschedule();
      } else {
        // A process is waiting directly. The communication can proceed.
        for (int i = 0; i < A; i++)
          writeByte(dest + i, readByte(C + i));
        // Reschedule the other thread.
        schedule(chan_value);

        // Reset the channel.
        write(B, NotProcess);
      }
    }
  }
}

// Output word.
DEFINE_INDIRECT(OUTWORD) {
  write(Wptr, A);
  A = 4;
  C = Wptr;

  INDIRECT(OUT);
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
  if (isExternalChannelReader(A)) {
    channelReader(A).reset();
  } else if (isExternalChannelWriter(A)) {
    channelWriter(A).reset();
  } else {
    write(A, NotProcess);
  }
}

// Reverse.
DEFINE_INDIRECT(REV) {
  A ^= B;
  B ^= A;
  A ^= B;
}

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

// Store byte.
DEFINE_INDIRECT(SB) {
  writeByte(A, static_cast<int8_t>(B));
  A = C;
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

// Stop process.
DEFINE_INDIRECT(STOPP) {
  stop();
}

// Subtract.
DEFINE_INDIRECT(SUB) {
  // TODO: Check for overflow.
  A = B - A;
  B = C;
}

// Word subscript.
DEFINE_INDIRECT(WSUB) {
  A += B * 4;
  B = C;
}

// Exclusive or.
DEFINE_INDIRECT(XOR) {
  A = A ^ B;
  B = C;
}

///////////////////////////////////////////////////////////////////////////////
// META OPERATIONS BEGIN                                                     //
///////////////////////////////////////////////////////////////////////////////

// Print a byte to the console.
DEFINE_INDIRECT(PUTC) {
  char c = static_cast<char>(pop());
  putc(c, stdout);
}

// Print A bytes pointed to by B to the console.
DEFINE_INDIRECT(PUTS) {
  for (int i = 0; i < A; i++) {
    char c = static_cast<char>(readByte(B + i));
    putc(c, stdout);
  }
}

// Print a word to the console (decimal).
DEFINE_INDIRECT(PRINTDEC) {
  printf("%d", pop());
}

// Print a word to the console (hexadecimal).
DEFINE_INDIRECT(PRINTHEX) {
  printf("%x", static_cast<uint32_t>(pop()));
}

// Print A words from the array pointed to by B.
DEFINE_INDIRECT(PRINTR) {
  for (int i = 0; i < A; i++) {
    printf("[0x%08x] = %d\n", B + 4 * i, read(B + 4 * i));
  }
}
