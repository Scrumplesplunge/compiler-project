#include "VM.h"

#include "../util.h"

#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>

using namespace std;

// COMPILE OPTIONS:
// DISABLE_BOUND_CHECKS - If defined, bound checks for the instruction memory
//                        and program memory will not be performed. This
//                        improves performance by a significant amount in
//                        optimised builds.
const int32_t VM::Enabling        = 0x80000001;
const int32_t VM::ExternalChannel = 0x80000002;
const int32_t VM::False           = 0x00000000;
const int32_t VM::MostNeg         = 0x80000000;
const int32_t VM::NoneSelected    = 0xFFFFFFFF;
const int32_t VM::NotProcess      = 0x80000000;
const int32_t VM::Ready           = 0x80000003;
const int32_t VM::True            = 0x00000001;
const int32_t VM::Waiting         = 0x80000002;

VM::VM(int32_t memory_start, unique_ptr<int32_t[]> memory, int memory_size,
       const char* bytecode, int bytecode_size)
    : bytecode_(bytecode), bytecode_size_(bytecode_size), memory_(move(memory)),
      memory_start_(memory_start), memory_size_(memory_size),
      memory_end_(memory_start + memory_size) {
  Wptr = memory_start;
}

void VM::run() {
  running_ = true;
  do {
    #ifdef DISABLE_BOUND_CHECKS
      #warning "Will not check that instruction pointer stays within code."
    #else
      if (Iptr < 0)
        throw runtime_error("Error: Iptr outside code (negative).");
      if (bytecode_size_ <= Iptr)
        throw runtime_error("Error: Iptr outside code (positive).");
    #endif

    int8_t code = fetch();
    Direct op = static_cast<Direct>(code & 0xF0);
    int32_t argument = code & 0xF;

    performDirect(op, argument);
  } while (running_);
}

void VM::step(bool debug) {
  if (debug) cerr << toString() << "\t";
  
  int8_t code = fetch();
  Direct op = static_cast<Direct>(code & 0xF0);
  int32_t argument = code & 0xF;
  while (op == PFIX || op == NFIX) {
    performDirect(op, argument);

    code = fetch();
    op = static_cast<Direct>(code & 0xF0);
    argument = code & 0xF;
  }

  if (debug) {
    int32_t final_operand = Oreg | argument;
    if (op == OPR) {
      // Show the indirect instruction.
      Indirect code = static_cast<Indirect>(final_operand);
      cerr << ::toString(code) << "\n";
    } else {
      // Show the direct instruction.
      cerr << ::toString(op) << " " << to_string(final_operand) << "\n";
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
          addressString(memory_start_));
    } else if (address >= memory_end_) {
      throw runtime_error(
          "Address " + addressString(address) + " >= " +
          addressString(memory_end_));
    }
  #endif

  address -= memory_start_;
  
  // These type conversions are necessary: We want a logical right-shift
  // (which requires an unsigned argument).
  address = static_cast<int32_t>(static_cast<uint32_t>(address) >> 2);
  return memory_[address];
}

int32_t VM::read(int32_t address) {
  int32_t value = (*this)[address];
  return value;
}

int8_t VM::readByte(int32_t address) {
  int32_t word = read(address);
  // Extract the byte. The least significant byte has the lowest address.
  // These type conversions are necessary: We want a logical right-shift (which
  // requires an unsigned argument).
  return
      static_cast<int8_t>(static_cast<uint32_t>(word) >> (8 * (address % 4)));
}

void VM::write(int32_t address, int32_t value) {
  (*this)[address] = value;
}

void VM::writeByte(int32_t address, int8_t value) {
  // Read the current word value, substitute the appropriate byte with the new
  // value, and write back to memory.
  int32_t word = read(address);
  int shift = 8 * (address % 4);
  int32_t mask = ~(0xFF << shift);
  write(address, (word & mask) | (value << shift));
}

void VM::enqueueProcess(int32_t desc) {
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

int32_t VM::dequeueProcess() {
  int32_t desc = FptrReg;

  // Update the front pointer if necessary.
  if (desc != NotProcess) FptrReg = read(makeWptr(desc) - 8);

  return desc;
}

int8_t VM::fetch() {
  op_count_++;
  return bytecode_[Iptr++];
}

void VM::performDirect(Direct op, int32_t argument) {
  Oreg |= argument;
  switch (op) {
    // See VMDirect.cc
    case ADC:   DIRECT(ADC);   break;
    case AJW:   DIRECT(AJW);   break;
    case CALL:  DIRECT(CALL);  break;
    case CJ:    DIRECT(CJ);    break;
    case EQC:   DIRECT(EQC);   break;
    case J:     DIRECT(J);     break;
    case LDC:   DIRECT(LDC);   break;
    case LDL:   DIRECT(LDL);   break;
    case LDLP:  DIRECT(LDLP);  break;
    case LDNL:  DIRECT(LDNL);  break;
    case LDNLP: DIRECT(LDNLP); break;
    case NFIX:  DIRECT(NFIX);  break;
    case OPR:   DIRECT(OPR);   break;
    case PFIX:  DIRECT(PFIX);  break;
    case STL:   DIRECT(STL);   break;
    case STNL:  DIRECT(STNL);  break;
    default:
      // This should be unreachable.
      throw logic_error("Unrecognised direct instruction: " + ::toString(op) +
                        "(" + to_string(op) + ")");
  }
}

void VM::performIndirect(Indirect op) {
  switch (op) {
    // See VMIndirect.cc
    case ADD:     INDIRECT(ADD);     break;
    case ALT:     INDIRECT(ALT);     break;
    case ALTEND:  INDIRECT(ALTEND);  break;
    case ALTWT:   INDIRECT(ALTWT);   break;
    case AND:     INDIRECT(AND);     break;
    case DIFF:    INDIRECT(DIFF);    break;
    case DISC:    INDIRECT(DISC);    break;
    case DISS:    INDIRECT(DISS);    break;
    case DIV:     INDIRECT(DIV);     break;
    case DUP:     INDIRECT(DUP);     break;
    case ENBC:    INDIRECT(ENBC);    break;
    case ENBS:    INDIRECT(ENBS);    break;
    case ENDP:    INDIRECT(ENDP);    break;
    case GT:      INDIRECT(GT);      break;
    case IN:      INDIRECT(IN);      break;
    case LB:      INDIRECT(LB);      break;
    case LDPI:    INDIRECT(LDPI);    break;
    case LEND:    INDIRECT(LEND);    break;
    case MINT:    INDIRECT(MINT);    break;
    case MUL:     INDIRECT(MUL);     break;
    case NOT:     INDIRECT(NOT);     break;
    case OR:      INDIRECT(OR);      break;
    case OUT:     INDIRECT(OUT);     break;
    case OUTWORD: INDIRECT(OUTWORD); break;
    case REM:     INDIRECT(REM);     break;
    case RESETCH: INDIRECT(RESETCH); break;
    case REV:     INDIRECT(REV);     break;
    case RUNP:    INDIRECT(RUNP);    break;
    case SB:      INDIRECT(SB);      break;
    case SHL:     INDIRECT(SHL);     break;
    case SHR:     INDIRECT(SHR);     break;
    case STARTP:  INDIRECT(STARTP);  break;
    case STOPP:   INDIRECT(STOPP);   break;
    case SUB:     INDIRECT(SUB);     break;
    case WSUB:    INDIRECT(WSUB);    break;
    case XOR:     INDIRECT(XOR);     break;

    // META operations.
    case PUTC:     INDIRECT(PUTC);     break;
    case PUTS:     INDIRECT(PUTS);     break;
    case PRINTDEC: INDIRECT(PRINTDEC); break;
    case PRINTHEX: INDIRECT(PRINTHEX); break;
    case PRINTR:   INDIRECT(PRINTR);   break;

    default:
      throw logic_error("Unimplemented indirect instruction: " + ::toString(op) +
                        "(" + to_string(op) + ")");
  }
}

bool VM::isExternalChannelReader(int32_t address) {
  return channel_readers_.count(address) == 1;
}

bool VM::isExternalChannelWriter(int32_t address) {
  return channel_writers_.count(address) == 1;
}

ChannelReader& VM::channelReader(int32_t address) {
  auto i = channel_readers_.find(address);
  if (i == channel_readers_.end()) {
    throw runtime_error(
        "Address " + addressString(address) +
        " is not associated with an external channel (reader).");
  }
  return *i->second;
}

ChannelWriter& VM::channelWriter(int32_t address) {
  auto i = channel_writers_.find(address);
  if (i == channel_writers_.end()) {
    throw runtime_error(
        "Address " + addressString(address) +
        " is not associated with an external channel (writer).");
  }
  return *i->second;
}

void VM::deschedule() {
  write(Wptr - 4, Iptr);        // Save the instruction pointer.
  resumeNext();
}

void VM::schedule(int32_t desc) {
  int32_t new_Wptr = makeWptr(desc);

  // Set the next pointer of the new process.
  write(new_Wptr - 8, NotProcess);
  enqueueProcess(desc);
}

void VM::schedule() {
  write(Wptr - 4, Iptr);        // Save the instruction pointer.
  write(Wptr - 8, NotProcess);  // This process is at the back of the queue.
  schedule(makeWdesc(Wptr));
}

void VM::resumeNext() {
  int32_t next_Wdesc;
  if (FptrReg != NotProcess) {
    next_Wdesc = dequeueProcess();
  } else {
    // All processes have stopped. Exit gracefully.
    running_ = false;
    return;
  }
  Wptr = makeWptr(next_Wdesc);
  Iptr = read(Wptr - 4);
}

void VM::yield() {
  // Yield only if the process has been running for long enough.
  if (op_count_ >= next_yield_) {
    next_yield_ = op_count_ + time_slice_;
    schedule();
    resumeNext();
  }
}

void VM::stop() {
  deschedule();
}
