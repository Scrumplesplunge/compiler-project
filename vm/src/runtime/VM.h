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
      MostNeg,          // Most negative value.
      NoneSelected,     // Value representing no branch selected.
      NotProcess,       // Value representing no process.
      Ready,            // Ready state of alternative.
      True,             // Truth values.
      Waiting;          // Waiting state of alternative.

  // Construct a VM with the given block of memory available as RAM. Note that
  // memory_size should be in *bytes*, and that the memory will be loaded such
  // that index 0 is at address MemStart.
  VM(int32_t memory_start, std::unique_ptr<int32_t[]> memory, int memory_size,
     const char* bytecode, int bytecode_size);

  // Run the virtual machine until all processes stop.
  void run();

  // Begin the program. This should be called before stepping.
  void begin() { running_ = true; }

  // Perform a single instruction. Note that PFIX and NFIX instructions do not
  // count on their own and are included as part of their summary instruction.
  // If the debug flag is set, this will display the state of the registers
  // *before* executing the instruction, and will print the instruction that is
  // about to be performed.
  void step(bool debug = false);

  // Check whether the program is running.
  bool running() const { return running_; }

  std::string toString();

  int32_t& operator[](int32_t address);

  int32_t read(int32_t address);
  int8_t readByte(int32_t address);

  void write(int32_t address, int32_t value);
  void writeByte(int32_t address, int8_t value);

  // Returns the total number of cycles performed.
  int64_t cycles() const { return op_count_; }

  void set_instruction_pointer(int32_t value) { Iptr = value; }
  void set_workspace_pointer(int32_t value) { Wptr = value; }

 private:
  int8_t fetch();  // Fetch a single instruction for execution.
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

  // Convert between workspace descriptor and workspace pointer. In this case,
  // these are identity functions, but are abstracted in case this changes.
  static int32_t makeWdesc(int32_t Wptr) { return Wptr; }
  static int32_t makeWptr(int32_t Wdesc) { return Wdesc; }

  // Enqueues the process with the specified workspace descriptor.
  void enqueueProcess(int32_t desc);

  // Dequeues a process from the process queue and returns its workspace
  // descriptor.
  int32_t dequeueProcess();

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

  // Map from internal addresses to external channels.
  std::unordered_map<int32_t, std::unique_ptr<ChannelReader>> channel_readers_;
  std::unordered_map<int32_t, std::unique_ptr<ChannelWriter>> channel_writers_;
  
  // (Read-only) code.
  const char* bytecode_;
  int32_t bytecode_size_;
  bool running_ = false;

  // Main memory.
  std::unique_ptr<int32_t[]> memory_;
  int32_t memory_start_, memory_size_, memory_end_;  // In bytes.

  int32_t Wptr;                 // Workspace pointer.
  int32_t Iptr = 0;             // Instruction pointer.
  int32_t A = 0, B = 0, C = 0;  // Register stack.
  int32_t Oreg = 0;             // Operand register.

  int32_t BptrReg = NotProcess; // Back pointers for process queues.
  int32_t FptrReg = NotProcess; // Front pointers for process queues.

  // Number of operations executed since the last successful yield.
  int64_t op_count_ = 0;              // Number of cycles performed.
  int64_t next_yield_ = time_slice_;  // Cycle at which to allow yielding.
  int64_t time_slice_ = 64;           // Min. number of cycles between slices.
};
