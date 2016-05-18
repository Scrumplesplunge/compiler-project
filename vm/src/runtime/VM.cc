#include "VM.h"

#include "../util.h"

#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>
#include <util/atomic_output.h>

using namespace std;

const int32_t VM::Enabling        = 0x80000001;
const int32_t VM::ExternalChannel = 0x80000002;
const int32_t VM::False           = 0x00000000;
const int32_t VM::MostNeg         = 0x80000000;
const int32_t VM::NoneSelected    = 0xFFFFFFFF;
const int32_t VM::NotProcess      = 0x80000000;
const int32_t VM::Ready           = 0x80000003;
const int32_t VM::TopWptr         = 0x7FFFFFF8;
const int32_t VM::True            = 0x00000001;
const int32_t VM::Waiting         = 0x80000002;

void VM::encodeStatic(const string& static_bytes, int32_t* static_words) {
  int32_t n = static_bytes.length();
  for (int32_t j = 0; j < n; j += 4) {
    uint32_t accumulator = 0;
    for (int32_t i = 0; i < 4 && i + j < n; i++)
      accumulator |= static_cast<uint32_t>(static_bytes[i + j]) << (8 * i);
    static_words[j / 4] = static_cast<int32_t>(accumulator);
  }
}

VM::VM(int32_t memory_start, int32_t memory_size,
       const int32_t* static_data, int32_t static_size,
       const char* bytecode, int32_t bytecode_size)
    : static_data_(static_data), static_end_(VM::MostNeg + static_size),
      bytecode_(bytecode), bytecode_size_(bytecode_size),
      memory_(new int32_t[(memory_size + 3) / 4]), memory_start_(memory_start),
      memory_size_(memory_size), memory_end_(memory_start + memory_size) {
  Wptr = memory_start;
}

void VM::run() {
  running_ = true;
  do {
    #ifdef DISABLE_BOUND_CHECKS
      #warning "Will not check that instruction pointer stays within code."
    #else
      if (Iptr < 0)
        throw runtime_error("Error: Iptr outside code: " + to_string(Iptr));
      if (bytecode_size_ <= Iptr)
        throw runtime_error("Error: Iptr outside code: " + to_string(Iptr));
    #endif

    int8_t code = fetch();
    Direct op = static_cast<Direct>(code & 0xF0);
    int32_t argument = code & 0xF;

    performDirect(op, argument);
  } while (running_);
}

void VM::step() {
  string registers;
  if (options::debug)
    registers = toString();
  
  int8_t code = fetch();
  Direct op = static_cast<Direct>(code & 0xF0);
  int32_t argument = code & 0xF;
  while (op == PFIX || op == NFIX) {
    performDirect(op, argument);

    code = fetch();
    op = static_cast<Direct>(code & 0xF0);
    argument = code & 0xF;
  }

  if (options::debug) {
    int32_t final_operand = Oreg | argument;
    if (op == OPR) {
      // Show the indirect instruction.
      Indirect code = static_cast<Indirect>(final_operand);
      verr << registers << "\t" << ::toString(code) << "\n";
    } else {
      // Show the direct instruction.
      verr << registers << "\t" << ::toString(op) << " "
           << to_string(final_operand) << "\n";
    }
  }

  performDirect(op, argument);
}

string VM::toString() {
  stringstream out;
  out << "Wptr = " << addressString(Wptr) << "  "
      << "Iptr = " << addressString(Iptr) << "  "
      << "A = " << addressString(A) << "  "
      << "B = " << addressString(B) << "  "
      << "C = " << addressString(C);
  return out.str();
}

int32_t& VM::operator[](int32_t address) {
  #ifdef DISABLE_BOUND_CHECKS
    #warning "Will not check that memory access stays within allocated memory."
  #else
    if (address < memory_start_) {
      throw runtime_error(
          "Address " + addressString(address) + " < " +
          addressString(memory_start_) + "\n" + toString());
    } else if (address >= memory_end_) {
      throw runtime_error(
          "Address " + addressString(address) + " >= " +
          addressString(memory_end_));
    }
  #endif

  address -= memory_start_;
  
  // These type conversions are necessary: We want a logical right-shift
  // (which requires an unsigned argument).
  address = static_cast<int32_t>(static_cast<uint32_t>(address) / 4);
  return memory_[address];
}

int32_t VM::read(int32_t address) {
  if (__builtin_expect((address < static_end_), 0)) {
    // Access to static data.
    int32_t index =
        static_cast<int32_t>(static_cast<uint32_t>(address - MostNeg) / 4);
    return static_data_[index];
  }

  if (__builtin_expect((address >= memory_end_), 0)) {
    // Access to memory after end bound. Pass to external handler.
    return readAfterEnd(address);
  }

  int32_t value = (*this)[address];
  return value;
}

int8_t VM::readByte(int32_t address) {
  int32_t word = read(address);
  // Extract the byte. The least significant byte has the lowest address.
  // These type conversions are necessary: We want a logical right-shift (which
  // requires an unsigned argument).
  int shift = 8 * static_cast<int>(static_cast<uint32_t>(address) % 4);
  return static_cast<int8_t>(static_cast<uint32_t>(word) >> shift);
}

void VM::write(int32_t address, int32_t value) {
  (*this)[address] = value;
}

void VM::writeByte(int32_t address, int8_t value) {
  // Read the current word value, substitute the appropriate byte with the new
  // value, and write back to memory.
  int32_t word = read(address);
  int shift = 8 * static_cast<int>(static_cast<uint32_t>(address) % 4);
  int32_t mask = ~(0xFF << shift);
  write(address, (word & mask) | (value << shift));
}

void VM::importBytes(const char* data, int32_t address, int32_t length) {
  for (int32_t i = 0; i < length; i++) writeByte(address + i, data[i]);
}

void VM::exportBytes(char* data, int32_t address, int32_t length) {
  for (int32_t i = 0; i < length; i++) data[i] = readByte(address + i);
}

string VM::exportBytes(int32_t address, int32_t length) {
  string out;

  for (int32_t i = 0; i < length; i++)
    out.push_back(readByte(address + i));

  return out;
}

void VM::transferBytesFrom(VM* other, int32_t source_address,
                           int32_t destination_address, int32_t length) {
  for (int32_t i = 0; i < length; i++)
    writeByte(destination_address + i, other->readByte(source_address + i));
}

int32_t VM::readAfterEnd(int32_t address) {
  throw runtime_error("Address " + addressString(address) + " >= " +
                      addressString(memory_end_));
}

void VM::runSpecialInstruction(Indirect op) {
  throw runtime_error("Unimplemented instruction " + ::toString(op));
}

void VM::onEmptyProcessQueue(unique_lock<mutex>& lock) {
  running_ = false;
}

int8_t VM::fetch() {
  op_count_++;
  return bytecode_[Iptr++];
}

void VM::performDirect(Direct op, int32_t argument) {
  Oreg |= argument;
  switch (op) {
    // See VMDirect.cc
    #define DIRECT(type) case type: direct_##type(); break
    DIRECT(ADC);  DIRECT(AJW);  DIRECT(CALL);  DIRECT(CJ);
    DIRECT(EQC);  DIRECT(J);    DIRECT(LDC);   DIRECT(LDL);
    DIRECT(LDLP); DIRECT(LDNL); DIRECT(LDNLP); DIRECT(NFIX);
    DIRECT(OPR);  DIRECT(PFIX); DIRECT(STL);   DIRECT(STNL);
    #undef DIRECT
    default:
      // This should be unreachable.
      throw logic_error("Unrecognised direct instruction: " + ::toString(op) +
                        "(" + to_string(op) + ")");
  }
}

void VM::performIndirect(Indirect op) {
  switch (op) {
    // See VMIndirect.cc
    #define INDIRECT(type) case type: indirect_##type(); break
    INDIRECT(ADD);     INDIRECT(AND);   INDIRECT(DIFF); INDIRECT(DISS);
    INDIRECT(DIV);     INDIRECT(DUP);   INDIRECT(ENBS); INDIRECT(ENDP);
    INDIRECT(GT);      INDIRECT(LB);    INDIRECT(LDPI); INDIRECT(LEND);
    INDIRECT(MINT);    INDIRECT(MUL);   INDIRECT(NOT);  INDIRECT(OR);
    INDIRECT(OUTWORD); INDIRECT(REM);   INDIRECT(RET);  INDIRECT(REV);
    INDIRECT(RUNP);    INDIRECT(SB);    INDIRECT(SHL);  INDIRECT(SHR);
    INDIRECT(STARTP);  INDIRECT(STOPP); INDIRECT(SUB);  INDIRECT(WSUB);
    INDIRECT(XOR);

    // Debugging operations.
    INDIRECT(PUTC); INDIRECT(PUTS); INDIRECT(PRINTDEC); INDIRECT(PRINTHEX);
    INDIRECT(PRINTR);
    #undef INDIRECT

    #define VIRTUAL(type) case type: indirect_##type(); break
    VIRTUAL(ALT); VIRTUAL(ALTEND); VIRTUAL(ALTWT);
    VIRTUAL(DISC); VIRTUAL(ENBC); VIRTUAL(IN); VIRTUAL(OUT); VIRTUAL(RESETCH);
    #undef CHANOP
    default:
      // Additional instructions.
      runSpecialInstruction(op);
  }
}

void VM::deschedule(unique_lock<mutex>& lock) {
  write(Wptr - 4, Iptr);        // Save the instruction pointer.
  resumeNext(lock);
}

void VM::schedule(int32_t desc, unique_lock<mutex>& lock) {
  int32_t new_Wptr = makeWptr(desc);

  // Set the next pointer of the new process.
  write(new_Wptr - 8, NotProcess);
  enqueueProcess(desc, lock);
}

void VM::schedule(unique_lock<mutex>& lock) {
  write(Wptr - 4, Iptr);        // Save the instruction pointer.
  write(Wptr - 8, NotProcess);  // This process is at the back of the queue.
  schedule(makeWdesc(Wptr), lock);
}

void VM::resumeNext(unique_lock<mutex>& lock) {
  next_yield_ = op_count_ + time_slice_;
  int32_t next_Wdesc;
  if (FptrReg != NotProcess) {
    next_Wdesc = dequeueProcess(lock);
  } else {
    // All processes have stopped. Exit gracefully.
    return onEmptyProcessQueue(lock);
  }
  Wptr = makeWptr(next_Wdesc);
  Iptr = read(Wptr - 4);
}

void VM::yield(unique_lock<mutex>& lock) {
  // Yield only if the process has been running for long enough.
  if (op_count_ >= next_yield_) {
    schedule(lock);
    resumeNext(lock);
  }
}

void VM::stop(unique_lock<mutex>& lock) {
  deschedule(lock);
}

void VM::enqueueProcess(int32_t desc, unique_lock<mutex>& lock) {
  // Point the previous process (if it exists) at this one.
  if (FptrReg == NotProcess) {
    // Queue was empty. Make the new process the front process.
    FptrReg = desc;
  } else {
    // Queue is non-empty. Update the current last to point at this process.
    write(makeWptr(BptrReg) - 8, desc);
  }
  BptrReg = desc;
}

int32_t VM::dequeueProcess(unique_lock<mutex>& lock) {
  int32_t desc = FptrReg;

  // Update the front pointer if necessary.
  if (desc != NotProcess) FptrReg = read(makeWptr(desc) - 8);

  return desc;
}
