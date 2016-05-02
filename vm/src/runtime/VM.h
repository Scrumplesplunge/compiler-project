#pragma once

#include "../operations.h"
#include "channel.h"

#include <memory>
#include <stdint.h>
#include <unordered_map>

class VM {
 public:
  static const int32_t
      Enabling,         // Enabling state of alternative.
      ExternalChannel,  // Value stored in external channels.
      False,            // Truth values.
      MemStart,         // Start of user-accessible memory.
      MostNeg,          // Most negative value.
      NoneSelected,     // Value representing no branch selected.
      NotProcess,       // Value representing no process.
      Ready,            // Ready state of alternative.
      TimeNotSet,       // Tlink time not set.
      TimeSet,          // Tlink time set.
      True,             // Truth values.
      Waiting;          // Waiting state of alternative.

  // Construct a VM with the given block of memory available as RAM. At least
  // 4KiB must be provided (i.e. array size of at least 1024) and the execution
  // will begin at IPtr = 0. Note that memory_size should be in *bytes*.
  VM(std::unique_ptr<int32_t[]> memory, int memory_size,
     const std::string& bytecode);

  // Run the virtual machine until all processes stop.
  void run();

  std::string toString();

  int32_t& operator[](int32_t address);

  int32_t read(int32_t address);
  int8_t readByte(int32_t address);

  void write(int32_t address, int32_t value);
  void writeByte(int32_t address, int8_t value);

 private:
  void performDirect(Direct op, int32_t argument);
  void performIndirect(Indirect op);

  void setError();              // Set the error bit and (potentially) halt.

  // Return true if the address given is associated with an external channel.
  bool isExternalChannelReader(int32_t address);
  bool isExternalChannelWriter(int32_t address);

  // Returns a reference to the external channel which is associated with the
  // given address, or throws an exception if no such channel exists. Note that
  // since channels are simplex, the type of the channel must match the expected
  // type, or an exception will be thrown.
  ChannelReader& channelReader(int32_t address);
  ChannelWriter& channelWriter(int32_t address);

  void deschedule();            // Remove the running process from the process
                                // queue.
  void schedule(int32_t desc);  // Schedule the specified process.
  void schedule();              // Schedule the running process (ie. pause it).
  void resumeNext();           // Resume the next process.
  void yield();                 // schedule(); resumeNext();
  void stop();                  // End current process and resume the next.

  int32_t time();     // Return the time value for the current priority.
  int32_t Wdesc();    // Current process workspace descriptor.


  bool after(int32_t a, int32_t b);  // True iff time a is after time b.

  void enqueueProcess(int32_t desc);
  int32_t dequeueProcess(int32_t pri);

  void push(int32_t x);  // Push a value onto the register stack.
  int32_t pop();         // Pop a value off the register stack.

  // Direct operations.
  DECLARE_DIRECT(ADC);  DECLARE_DIRECT(AJW);   DECLARE_DIRECT(CALL);
  DECLARE_DIRECT(CJ);   DECLARE_DIRECT(EQC);   DECLARE_DIRECT(J);
  DECLARE_DIRECT(LDC);  DECLARE_DIRECT(LDL);   DECLARE_DIRECT(LDLP);
  DECLARE_DIRECT(LDNL); DECLARE_DIRECT(LDNLP); DECLARE_DIRECT(NFIX);
  DECLARE_DIRECT(OPR);  DECLARE_DIRECT(PFIX);  DECLARE_DIRECT(STL);
  DECLARE_DIRECT(STNL);

  // Utilised operations. These are the ones which the compiler generates.
  DECLARE_INDIRECT(ADD);   DECLARE_INDIRECT(ALT);     DECLARE_INDIRECT(ALTEND);
  DECLARE_INDIRECT(ALTWT); DECLARE_INDIRECT(AND);     DECLARE_INDIRECT(DIFF);
  DECLARE_INDIRECT(DISC);  DECLARE_INDIRECT(DISS);    DECLARE_INDIRECT(DIV);
  DECLARE_INDIRECT(DUP);   DECLARE_INDIRECT(ENBC);    DECLARE_INDIRECT(ENBS);
  DECLARE_INDIRECT(ENDP);  DECLARE_INDIRECT(GT);      DECLARE_INDIRECT(IN);
  DECLARE_INDIRECT(LB);    DECLARE_INDIRECT(LDPI);    DECLARE_INDIRECT(LEND);
  DECLARE_INDIRECT(MINT);  DECLARE_INDIRECT(MUL);     DECLARE_INDIRECT(NOT);
  DECLARE_INDIRECT(OR);    DECLARE_INDIRECT(OUT);     DECLARE_INDIRECT(OUTWORD);
  DECLARE_INDIRECT(REM);   DECLARE_INDIRECT(RESETCH); DECLARE_INDIRECT(REV);
  DECLARE_INDIRECT(RUNP);  DECLARE_INDIRECT(SB);      DECLARE_INDIRECT(SHL);
  DECLARE_INDIRECT(SHR);   DECLARE_INDIRECT(STARTP);  DECLARE_INDIRECT(STOPP);
  DECLARE_INDIRECT(SUB);   DECLARE_INDIRECT(WSUB);    DECLARE_INDIRECT(XOR);

  // Meta operations. These are intended for debugging only.
  DECLARE_INDIRECT(PUTC);     DECLARE_INDIRECT(PUTS);
  DECLARE_INDIRECT(PRINTDEC); DECLARE_INDIRECT(PRINTHEX);
  DECLARE_INDIRECT(PRINTR);

  // Unit operations. These are all executed via FPENTRY, and are thus
  // unimplemented.
  DECLARE_UNIT(FPUSQRTFIRST); DECLARE_UNIT(FPUSQRTSTEP);
  DECLARE_UNIT(FPUSQRTLAST);  DECLARE_UNIT(FPURP);
  DECLARE_UNIT(FPURM);

  // END OPERATIONS

  // Main memory.
  std::unique_ptr<int32_t[]> memory_;
  int32_t memory_size_;  // In bytes.

  // Map from internal addresses to external channels.
  std::unordered_map<int32_t, std::unique_ptr<ChannelReader>> channel_readers_;
  std::unordered_map<int32_t, std::unique_ptr<ChannelWriter>> channel_writers_;
  
  // (Read-only) code.
  const std::string& bytecode_;
  bool running_;

  int32_t Wptr;     // Workspace pointer.
  int32_t Iptr;     // Instruction pointer.
  int32_t A, B, C;  // Register stack.
  int32_t Oreg;     // Operand register.

  // Number of operations executed since the last successful yield.
  int32_t op_count_;
  int32_t yield_after_ = 64;

  int32_t priority;  // Priority of the current running process (0 or 1).

  // Each of the registers below correspond to either high priority (0) or low
  // priority (1) processes, and can only be accessed by processes of the
  // matching priority level.

  // ClockReg0 ticks once every microsecond. ClockReg1 ticks once every 64
  // microseconds.
  int32_t ClockReg[2];  // Clock registers for each priority.
  int32_t BptrReg[2];   // Back pointers for process queues.
  int32_t FptrReg[2];   // Front pointers for process queues.

  // Timer queue head pointers.
  int32_t TptrLoc[2];

  bool Error;        // Error bit.
  bool HaltOnError;  // Do we halt on errors?
};
