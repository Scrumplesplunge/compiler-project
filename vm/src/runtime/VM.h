#pragma once

#include "../operations.h"

#include <memory>
#include <stdint.h>

class VM {
 public:
  static const int32_t Enabling;      // Enabling state of alternative.
  static const int32_t False;         // Truth values.
  static const int32_t MemStart;      // Start of user-accessible memory.
  static const int32_t MostNeg;       // Most negative value.
  static const int32_t NoneSelected;  // Value representing no branch selected.
  static const int32_t NotProcess;    // Value representing no process.
  static const int32_t Ready;         // Ready state of alternative.
  static const int32_t TimeNotSet;    // Tlink time not set.
  static const int32_t TimeSet;       // Tlink time set.
  static const int32_t True;          // Truth values.
  static const int32_t Waiting;       // Waiting state of alternative.

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
  void performUnit(Unit op);

  void setError();              // Set the error bit and (potentially) halt.

  void deschedule();            // Remove the running process from the process queue.
  void schedule(int32_t desc);  // Schedule the specified process.
  void schedule();              // Schedule the running process (ie. pause it).
  void resumeNext();            // Resume the next process.
  void yield();                 // schedule(); resumeNext();
  void stop();                  // End current process and resume the next.

  int32_t time();     // Return the time value for the current priority.
  int32_t Wdesc();    // Current process workspace descriptor.

  // Return true if the address stored in the channel is the address of another
  // processes workspace.
  bool isExternalChannel(int32_t address);
  bool after(int32_t a, int32_t b);  // True iff time a is after time b.

  void enqueueProcess(int32_t desc);
  int32_t dequeueProcess(int32_t pri);

  void push(int32_t x);  // Push a value onto the register stack.
  int32_t pop();         // Pop a value off the register stack.

  void perform(const Operation& operation);

  // BEGIN OPERATIONS
  // Note that not all of these operations are implemented: operations which are
  // not implemented will cause an exception when they are executed.
  DECLARE_DIRECT(ADC);  DECLARE_DIRECT(AJW);   DECLARE_DIRECT(CALL);
  DECLARE_DIRECT(CJ);   DECLARE_DIRECT(EQC);   DECLARE_DIRECT(J);
  DECLARE_DIRECT(LDC);  DECLARE_DIRECT(LDL);   DECLARE_DIRECT(LDLP);
  DECLARE_DIRECT(LDNL); DECLARE_DIRECT(LDNLP); DECLARE_DIRECT(NFIX);
  DECLARE_DIRECT(OPR);  DECLARE_DIRECT(PFIX);  DECLARE_DIRECT(STL);
  DECLARE_DIRECT(STNL);

  DECLARE_INDIRECT(ADD);         DECLARE_INDIRECT(ALT);
  DECLARE_INDIRECT(ALTEND);      DECLARE_INDIRECT(ALTWT);
  DECLARE_INDIRECT(AND);         DECLARE_INDIRECT(BCNT);
  DECLARE_INDIRECT(BITCNT);      DECLARE_INDIRECT(BITREVNBITS);
  DECLARE_INDIRECT(BITREVWORD);  DECLARE_INDIRECT(BSUB);
  DECLARE_INDIRECT(CCNT1);       DECLARE_INDIRECT(CFLERR);
  DECLARE_INDIRECT(CLRHALTERR);  DECLARE_INDIRECT(CRCBYTE);
  DECLARE_INDIRECT(CRCWORD);     DECLARE_INDIRECT(CSNGL);
  DECLARE_INDIRECT(CSUB0);       DECLARE_INDIRECT(CWORD);
  DECLARE_INDIRECT(DIFF);        DECLARE_INDIRECT(DISC);
  DECLARE_INDIRECT(DISS);        DECLARE_INDIRECT(DIST);
  DECLARE_INDIRECT(DIV);         DECLARE_INDIRECT(DUP);
  DECLARE_INDIRECT(ENBC);        DECLARE_INDIRECT(ENBS);
  DECLARE_INDIRECT(ENBT);        DECLARE_INDIRECT(ENDP);
  DECLARE_INDIRECT(FMUL);        DECLARE_INDIRECT(FPADD);
  DECLARE_INDIRECT(FPB32TOR64);  DECLARE_INDIRECT(FPCHKERR);
  DECLARE_INDIRECT(FPDIV);       DECLARE_INDIRECT(FPDUP);
  DECLARE_INDIRECT(FPENTRY);     DECLARE_INDIRECT(FPEQ);
  DECLARE_INDIRECT(FPGT);        DECLARE_INDIRECT(FPI32TOR32);
  DECLARE_INDIRECT(FPI32TOR64);  DECLARE_INDIRECT(FPINT);
  DECLARE_INDIRECT(FPLDNLADDDB); DECLARE_INDIRECT(FPLDNLADDSN);
  DECLARE_INDIRECT(FPLDNLDB);    DECLARE_INDIRECT(FPLDNLDBI);
  DECLARE_INDIRECT(FPLDNLMULDB); DECLARE_INDIRECT(FPLDNLMULSN);
  DECLARE_INDIRECT(FPLDNLSN);    DECLARE_INDIRECT(FPLDNLSNI);
  DECLARE_INDIRECT(FPLDZERODB);  DECLARE_INDIRECT(FPLDZEROSN);
  DECLARE_INDIRECT(FPMUL);       DECLARE_INDIRECT(FPNAN);
  DECLARE_INDIRECT(FPNOTFINITE); DECLARE_INDIRECT(FPORDERED);
  DECLARE_INDIRECT(FPREMFIRST);  DECLARE_INDIRECT(FPREMSTEP);
  DECLARE_INDIRECT(FPREV);       DECLARE_INDIRECT(FPRTOI32);
  DECLARE_INDIRECT(FPSTNLDB);    DECLARE_INDIRECT(FPSTNLI32);
  DECLARE_INDIRECT(FPSTNLSN);    DECLARE_INDIRECT(FPSUB);
  DECLARE_INDIRECT(FPTESTERR);   DECLARE_INDIRECT(GAJW);
  DECLARE_INDIRECT(GCALL);       DECLARE_INDIRECT(GT);
  DECLARE_INDIRECT(IN);          DECLARE_INDIRECT(LADD);
  DECLARE_INDIRECT(LB);          DECLARE_INDIRECT(LDIFF);
  DECLARE_INDIRECT(LDINF);       DECLARE_INDIRECT(LDIV);
  DECLARE_INDIRECT(LDPI);        DECLARE_INDIRECT(LDPRI);
  DECLARE_INDIRECT(LDTIMER);     DECLARE_INDIRECT(LEND);
  DECLARE_INDIRECT(LMUL);        DECLARE_INDIRECT(LSHL);
  DECLARE_INDIRECT(LSHR);        DECLARE_INDIRECT(LSUB);
  DECLARE_INDIRECT(LSUM);        DECLARE_INDIRECT(MINT);
  DECLARE_INDIRECT(MOVE);        DECLARE_INDIRECT(MOVE2DALL);
  DECLARE_INDIRECT(MOVE2DINIT);  DECLARE_INDIRECT(MOVE2DNONZERO);
  DECLARE_INDIRECT(MOVE2DZERO);  DECLARE_INDIRECT(MUL);
  DECLARE_INDIRECT(NORM);        DECLARE_INDIRECT(NOT);
  DECLARE_INDIRECT(OR);          DECLARE_INDIRECT(OUT);
  DECLARE_INDIRECT(OUTBYTE);     DECLARE_INDIRECT(OUTWORD);
  DECLARE_INDIRECT(POSTNORMSN);  DECLARE_INDIRECT(PROD);
  DECLARE_INDIRECT(REM);         DECLARE_INDIRECT(RESETCH);
  DECLARE_INDIRECT(RET);         DECLARE_INDIRECT(REV);
  DECLARE_INDIRECT(ROUNDSN);     DECLARE_INDIRECT(RUNP);
  DECLARE_INDIRECT(SAVEH);       DECLARE_INDIRECT(SAVEL);
  DECLARE_INDIRECT(SB);          DECLARE_INDIRECT(SETERR);
  DECLARE_INDIRECT(SETHALTERR);  DECLARE_INDIRECT(SHL);
  DECLARE_INDIRECT(SHR);         DECLARE_INDIRECT(STARTP);
  DECLARE_INDIRECT(STHB);        DECLARE_INDIRECT(STHF);
  DECLARE_INDIRECT(STLB);        DECLARE_INDIRECT(STLF);
  DECLARE_INDIRECT(STOPERR);     DECLARE_INDIRECT(STOPP);
  DECLARE_INDIRECT(STTIMER);     DECLARE_INDIRECT(SUB);
  DECLARE_INDIRECT(SUM);         DECLARE_INDIRECT(TALT);
  DECLARE_INDIRECT(TALTWT);      DECLARE_INDIRECT(TESTERR);
  DECLARE_INDIRECT(TESTHALTERR); DECLARE_INDIRECT(TESTPRANAL);
  DECLARE_INDIRECT(TIN);         DECLARE_INDIRECT(UNPACKSN);
  DECLARE_INDIRECT(WCNT);        DECLARE_INDIRECT(WSUB);
  DECLARE_INDIRECT(WSUBDB);      DECLARE_INDIRECT(XDBLE);
  DECLARE_INDIRECT(XOR);         DECLARE_INDIRECT(XWORD);

  DECLARE_UNIT(FPUSQRTFIRST); DECLARE_UNIT(FPUSQRTSTEP);
  DECLARE_UNIT(FPUSQRTLAST);  DECLARE_UNIT(FPURP);
  DECLARE_UNIT(FPURM);

  // META OPERATIONS
  DECLARE_INDIRECT(PUTC);     DECLARE_INDIRECT(PUTS);
  DECLARE_INDIRECT(PRINTDEC); DECLARE_INDIRECT(PRINTHEX);
  DECLARE_INDIRECT(PRINTR);
  // END OPERATIONS

  // Main memory.
  std::unique_ptr<int32_t[]> memory_;
  int32_t memory_size_;  // In bytes.
  
  // (Read-only) code.
  const std::string& bytecode_;
  bool running_;

  int32_t Wptr;     // Workspace pointer.
  int32_t Iptr;     // Instruction pointer.
  int32_t A, B, C;  // Register stack.
  int32_t Oreg;     // Operand register.

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
