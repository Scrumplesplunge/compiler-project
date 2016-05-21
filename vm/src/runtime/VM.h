#pragma once

#include "Direct.h"
#include "Indirect.h"

#include <memory>
#include <mutex>
#include <stdint.h>
#include <unordered_map>

// COMPILE OPTIONS:
// DISABLE_BOUND_CHECKS - If defined, bound checks for the instruction memory
//                        and program memory will not be performed. This
//                        improves performance by a significant amount in
//                        optimised builds.
// DISABLE_MUTEX        - If defined, the VM will not protect itself against
//                        concurrent access.

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
      TopWptr,          // Highest workspace pointer.
      True,             // Truth values.
      Waiting;          // Waiting state of alternative.

  static void encodeStatic(
      const std::string& static_bytes, int32_t* static_words);

  // Construct a VM with the given block of memory available as RAM. Note that
  // memory_size should be in *bytes*, and that the memory will be loaded such
  // that index 0 is at address MemStart.
  VM(int32_t memory_start, int32_t memory_size,
     const int32_t* static_data, int32_t static_size,
     const char* bytecode, int32_t bytecode_size);
  virtual ~VM() = default;

  // Run the virtual machine until all processes stop.
  void run();

  // Begin the program. This should be called before stepping.
  void begin() { running_ = true; }

  // Perform a single instruction. Note that PFIX and NFIX instructions do not
  // count on their own and are included as part of their summary instruction.
  // If the debug flag is set, this will display the state of the registers
  // *before* executing the instruction, and will print the instruction that is
  // about to be performed.
  void step();

  // Check whether the program is running.
  bool running() const { return running_; }

  virtual std::string toString();

  int32_t& operator[](int32_t address);

  int32_t read(int32_t address);
  int8_t readByte(int32_t address);

  void write(int32_t address, int32_t value);
  void writeByte(int32_t address, int8_t value);

  void importBytes(const char* data, int32_t address, int32_t length);
  void exportBytes(char* data, int32_t address, int32_t length);
  std::string exportBytes(int32_t address, int32_t length);

  void transferBytesFrom(VM* other, int32_t source_address,
                         int32_t destination_address, int32_t length);

  // Returns the total number of cycles performed.
  int64_t cycles() const { return op_count_; }

  void set_instruction_pointer(int32_t value) { Iptr = value; }
  void set_workspace_pointer(int32_t value) { Wptr = value; }

 protected:
  // Handler used when a read has an address larger than memory_end_.
  virtual int32_t readAfterEnd(int32_t address);

  // Handler used when an unrecognised indirect instructino is executed.
  virtual void runSpecialInstruction(Indirect op);

  int8_t fetch();  // Fetch a single instruction for execution.
  void performDirect(Direct op, int32_t argument);
  void performIndirect(Indirect op);

  // Handler used when the the only active process stops.
  #ifdef DISABLE_MUTEX
    typedef void* lock_t;
  #else
    typedef std::unique_lock<std::mutex>& lock_t;
  #endif 

  virtual void onEmptyProcessQueue(lock_t lock);

  // Remove the running process from the process queue.
  void deschedule(lock_t lock);
  // Schedule the specified process.
  void schedule(int32_t desc, lock_t lock);
  // Schedule the running process (ie. pause it).
  void schedule(lock_t lock);
  // Resume the next process.
  void resumeNext(lock_t lock);
  // Conditionally schedule(); resumeNext();
  void yield(lock_t lock);
  // End current process and resume the next.
  void stop(lock_t lock);

  // Convert between workspace descriptor and workspace pointer. In this case,
  // these are identity functions, but are abstracted in case this changes.
  static int32_t makeWdesc(int32_t Wptr) { return Wptr; }
  static int32_t makeWptr(int32_t Wdesc) { return Wdesc; }

  // Enqueues the process with the specified workspace descriptor.
  void enqueueProcess(int32_t desc, lock_t lock);

  // Dequeues a process from the process queue and returns its workspace
  // descriptor.
  int32_t dequeueProcess(lock_t lock);

  // Direct operations.
  #define DIRECT(type) void direct_##type()
  DIRECT(ADC);   DIRECT(AJW);   DIRECT(CALL);  DIRECT(CJ);
  DIRECT(EQC);   DIRECT(J);     DIRECT(LDC);   DIRECT(LDL);
  DIRECT(LDLP);  DIRECT(LDNL);  DIRECT(LDNLP); DIRECT(NFIX);
  DIRECT(OPR);   DIRECT(PFIX);  DIRECT(STL);   DIRECT(STNL);
  #undef DIRECT

  // Indirect operations.
  #define INDIRECT(type) void indirect_##type();
  // Standard operations.
  INDIRECT(ADD);
  INDIRECT(AND);    INDIRECT(DIFF); INDIRECT(DISS);     INDIRECT(DIV);
  INDIRECT(DUP);    INDIRECT(ENBS); INDIRECT(ENDP);     INDIRECT(GT);
  INDIRECT(LB);     INDIRECT(LDPI); INDIRECT(LEND);     INDIRECT(MINT);
  INDIRECT(MUL);    INDIRECT(NOT);  INDIRECT(OR);       INDIRECT(OUTWORD);
  INDIRECT(REM);    INDIRECT(RET);  INDIRECT(REV);      INDIRECT(RUNP);
  INDIRECT(SB);     INDIRECT(SHL);  INDIRECT(SHR);      INDIRECT(STARTP);
  INDIRECT(STOPP);  INDIRECT(SUB);  INDIRECT(WSUB);     INDIRECT(XOR);

  // Debugging operations.
  INDIRECT(PUTC);   INDIRECT(PUTS); INDIRECT(PRINTDEC); INDIRECT(PRINTHEX);
  INDIRECT(PRINTR);
  #undef INDIRECT

  // Some indirect operations need to be overridable.
  #define VIRTUAL(type) virtual void indirect_##type()
  VIRTUAL(ALT);  VIRTUAL(ALTEND); VIRTUAL(ALTWT);  VIRTUAL(DISC);
  VIRTUAL(ENBC); VIRTUAL(IN);     VIRTUAL(OUT);    VIRTUAL(RESETCH);
  #undef VIRTUAL

  // (Read-only) data.
  const int32_t* static_data_;
  int32_t static_end_;

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

  #ifndef DISABLE_MUTEX
  std::mutex queue_mu_;
  #endif

  int32_t BptrReg = NotProcess; // Back pointers for process queues.
  int32_t FptrReg = NotProcess; // Front pointers for process queues.

  int64_t time_slice_ = 64;           // Min. number of cycles between slices.
  int64_t op_count_ = 0;              // Number of cycles performed.
  int64_t next_yield_ = time_slice_;  // Cycle at which to allow yielding.
};
