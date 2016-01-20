#include "State.h"

#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>

using namespace std;

const int32_t State::Enabling     = 0x80000001;
const int32_t State::False        = 0x00000000;
const int32_t State::MemStart     = 0x80000070;
const int32_t State::MostNeg      = 0x80000000;
const int32_t State::NoneSelected = 0xFFFFFFFF;
const int32_t State::NotProcess   = 0x80000000;
const int32_t State::Ready        = 0x80000003;
const int32_t State::TimeNotSet   = 0xFFFFFFFF;
const int32_t State::TimeSet      = 0xFFFFFFFE;
const int32_t State::True         = 0xFFFFFFFF;
const int32_t State::Waiting      = 0x80000002;

static string addressString(int32_t address) {
  uint32_t raw = static_cast<uint32_t>(address);
  const char digits[] = "0123456789abcdef";
  char out[8];
  for (int i = 8; i-- > 0;) {
    out[i] = digits[raw & 0xF];
    raw >>= 4;
  }
  return "0x" + string(out, 8);
}

State::State(int32_t memory_size)
    : memory_size_(memory_size),
      memory_(new int32_t[(memory_size + 3) / 4]) {
  if (memory_size < 0x1000)
    throw logic_error("Minimum memory size is 4K.");

  // Initialize the registers.
  Wptr = MemStart;
  Iptr = 0;
  A = B = C = Oreg = ClockReg[0] = ClockReg[1] = 0;

  priority = 0;
  Error = HaltOnError = false;
  
  BptrReg[0] = BptrReg[1] = NotProcess;
  FptrReg[0] = FptrReg[1] = NotProcess;
  TptrLoc[0] = TptrLoc[1] = NotProcess;
}

string State::toString() {
  stringstream out;
  out << "Wptr = " << addressString(Wptr) << "  "
      << "Iptr = " << addressString(Iptr) << "  "
      << "A = " << A << "  "
      << "B = " << B << "  "
      << "C = " << C;
  return out.str();
}

void State::enqueueProcess(int32_t desc) {
  int32_t pri = desc & 0x3;
  if (pri != 0 && pri != 1)
    throw runtime_error("Bad priority level: " + to_string(pri));

  // Point the previous process (if it exists) at this one.
  if (FptrReg[pri] == NotProcess) {
    // Queue was empty. Make the new process the front process.
    FptrReg[pri] = desc;
  } else {
    // Queue is non-empty. Update the current last to point at this process.
    write((BptrReg[pri] & ~0x3) - 8, desc);
  }
  BptrReg[pri] = desc;
}

int32_t State::dequeueProcess(int32_t pri) {
  if (pri != 0 && pri != 1)
    throw runtime_error("Bad priority level: " + to_string(pri));

  int32_t desc = FptrReg[pri];

  // Update the front pointer if necessary.
  if (desc != NotProcess)
    FptrReg[pri] = read((desc & ~0x3) - 8);

  return desc;
}

void State::push(int32_t x) {
  C = B;
  B = A;
  A = x;
}

int32_t State::pop() {
  int32_t x = A;
  A = B;
  B = C;
  return x;
}

int32_t& State::operator[](int32_t address) {
  if (address >= memory_size_ + MostNeg && false) {
    throw runtime_error(
        "Address " + addressString(address) + " >= " +
        to_string(memory_size_) + " + " + addressString(MostNeg) + " = " +
        addressString(memory_size_ + MostNeg));
  }

  address -= MostNeg;
  
  // These type conversions are necessary: We want a logical right-shift
  // (which requires an unsigned argument).
  address = static_cast<int32_t>(static_cast<uint32_t>(address) >> 2);
  return memory_[address];
}

int32_t State::read(int32_t address) {
  return (*this)[address];
}

int8_t State::readByte(int32_t address) {
  int32_t word = read(address);
  // Extract the byte. The least significant byte has the lowest address.
  // These type conversions are necessary: We want a logical right-shift (which
  // requires an unsigned argument).
  return
      static_cast<int8_t>(static_cast<uint32_t>(word) >> (8 * (address & 0x3)));
}

void State::write(int32_t address, int32_t value) {
  (*this)[address] = value;
}

void State::writeByte(int32_t address, int8_t value) {
  // Read the current word value, substitute the appropriate byte with the new
  // value, and write back to memory.
  int32_t word = read(address);
  int shift = 8 * (address & 0x3);
  int32_t mask = ~(0xFF << shift);
  write(address, (word & mask) | (value << shift));
}

void State::perform(const Operation& operation) {
  Iptr++;

  switch (operation.type()) {
    case Operation::DIRECT: {
      Direct op; int32_t argument;
      if (!operation.getDirect(&op, &argument))
        throw logic_error("This should never happen.");
      performDirect(op, argument);
      break;
    }
    case Operation::INDIRECT: {
      Indirect op;
      if (!operation.getIndirect(&op))
        throw logic_error("This should never happen.");
      performIndirect(op);
      break;
    }
    case Operation::UNIT: {
      Unit op;
      if (!operation.getUnit(&op))
        throw logic_error("This should never happen.");
      performUnit(op);
      break;
    }
    default:
      throw logic_error("Operation type is none of DIRECT, INDIRECT, or UNIT.");
  }
}

void State::performDirect(Direct op, int32_t argument) {
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
      throw logic_error("Unrecognised direct instruction: " + ::toString(op) +
                        "(" + to_string(op) + ")");
  }
}

void State::performIndirect(Indirect op) {
  switch (op) {
    // See VMIndirect.cc
    case ADD:           INDIRECT(ADD);            break;
    case ALT:           INDIRECT(ALT);            break;
    case ALTEND:        INDIRECT(ALTEND);         break;
    case ALTWT:         INDIRECT(ALTWT);          break;
    case AND:           INDIRECT(AND);            break;
    case BCNT:          INDIRECT(BCNT);           break;
    case BSUB:          INDIRECT(BSUB);           break;
    case CCNT1:         INDIRECT(CCNT1);          break;
    case CFLERR:        INDIRECT(CFLERR);         break;
    case CLRHALTERR:    INDIRECT(CLRHALTERR);     break;
    case CRCBYTE:       INDIRECT(CRCBYTE);        break;
    case CRCWORD:       INDIRECT(CRCWORD);        break;
    case CSNGL:         INDIRECT(CSNGL);          break;
    case CSUB0:         INDIRECT(CSUB0);          break;
    case CWORD:         INDIRECT(CWORD);          break;
    case DIFF:          INDIRECT(DIFF);           break;
    case DISC:          INDIRECT(DISC);           break;
    case DISS:          INDIRECT(DISS);           break;
    case DIST:          INDIRECT(DIST);           break;
    case DIV:           INDIRECT(DIV);            break;
    case DUP:           INDIRECT(DUP);            break;
    case ENBC:          INDIRECT(ENBC);           break;
    case ENBS:          INDIRECT(ENBS);           break;
    case ENBT:          INDIRECT(ENBT);           break;
    case ENDP:          INDIRECT(ENDP);           break;
    case FMUL:          INDIRECT(FMUL);           break;
    case FPADD:         INDIRECT(FPADD);          break;
    case FPB32TOR64:    INDIRECT(FPB32TOR64);     break;
    case FPCHKERR:      INDIRECT(FPCHKERR);       break;
    case FPDIV:         INDIRECT(FPDIV);          break;
    case FPDUP:         INDIRECT(FPDUP);          break;
    case FPENTRY:       INDIRECT(FPENTRY);        break;
    case FPEQ:          INDIRECT(FPEQ);           break;
    case FPGT:          INDIRECT(FPGT);           break;
    case FPI32TOR32:    INDIRECT(FPI32TOR32);     break;
    case FPI32TOR64:    INDIRECT(FPI32TOR64);     break;
    case FPINT:         INDIRECT(FPINT);          break;
    case FPLDNLADDDB:   INDIRECT(FPLDNLADDDB);    break;
    case FPLDNLADDSN:   INDIRECT(FPLDNLADDSN);    break;
    case FPLDNLDB:      INDIRECT(FPLDNLDB);       break;
    case FPLDNLDBI:     INDIRECT(FPLDNLDBI);      break;
    case FPLDNLMULDB:   INDIRECT(FPLDNLMULDB);    break;
    case FPLDNLMULSN:   INDIRECT(FPLDNLMULSN);    break;
    case FPLDNLSN:      INDIRECT(FPLDNLSN);       break;
    case FPLDNLSNI:     INDIRECT(FPLDNLSNI);      break;
    case FPLDZERODB:    INDIRECT(FPLDZERODB);     break;
    case FPLDZEROSN:    INDIRECT(FPLDZEROSN);     break;
    case FPMUL:         INDIRECT(FPMUL);          break;
    case FPNAN:         INDIRECT(FPNAN);          break;
    case FPNOTFINITE:   INDIRECT(FPNOTFINITE);    break;
    case FPORDERED:     INDIRECT(FPORDERED);      break;
    case FPREMFIRST:    INDIRECT(FPREMFIRST);     break;
    case FPREMSTEP:     INDIRECT(FPREMSTEP);      break;
    case FPREV:         INDIRECT(FPREV);          break;
    case FPRTOI32:      INDIRECT(FPRTOI32);       break;
    case FPSTNLDB:      INDIRECT(FPSTNLDB);       break;
    case FPSTNLI32:     INDIRECT(FPSTNLI32);      break;
    case FPSTNLSN:      INDIRECT(FPSTNLSN);       break;
    case FPSUB:         INDIRECT(FPSUB);          break;
    case FPTESTERR:     INDIRECT(FPTESTERR);      break;
    case GAJW:          INDIRECT(GAJW);           break;
    case GCALL:         INDIRECT(GCALL);          break;
    case GT:            INDIRECT(GT);             break;
    case IN:            INDIRECT(IN);             break;
    case LADD:          INDIRECT(LADD);           break;
    case LB:            INDIRECT(LB);             break;
    case LDIFF:         INDIRECT(LDIFF);          break;
    case LDINF:         INDIRECT(LDINF);          break;
    case LDIV:          INDIRECT(LDIV);           break;
    case LDPI:          INDIRECT(LDPI);           break;
    case LDPRI:         INDIRECT(LDPRI);          break;
    case LDTIMER:       INDIRECT(LDTIMER);        break;
    case LEND:          INDIRECT(LEND);           break;
    case LSHL:          INDIRECT(LSHL);           break;
    case LSHR:          INDIRECT(LSHR);           break;
    case LSUB:          INDIRECT(LSUB);           break;
    case LSUM:          INDIRECT(LSUM);           break;
    case MINT:          INDIRECT(MINT);           break;
    case MOVE:          INDIRECT(MOVE);           break;
    case MOVE2DALL:     INDIRECT(MOVE2DALL);      break;
    case MOVE2DINIT:    INDIRECT(MOVE2DINIT);     break;
    case MOVE2DNONZERO: INDIRECT(MOVE2DNONZERO);  break;
    case MOVE2DZERO:    INDIRECT(MOVE2DZERO);     break;
    case MUL:           INDIRECT(MUL);            break;
    case NORM:          INDIRECT(NORM);           break;
    case NOT:           INDIRECT(NOT);            break;
    case OR:            INDIRECT(OR);             break;
    case OUT:           INDIRECT(OUT);            break;
    case OUTBYTE:       INDIRECT(OUTBYTE);        break;
    case OUTWORD:       INDIRECT(OUTWORD);        break;
    case POSTNORMSN:    INDIRECT(POSTNORMSN);     break;
    case PROD:          INDIRECT(PROD);           break;
    case REM:           INDIRECT(REM);            break;
    case RESETCH:       INDIRECT(RESETCH);        break;
    case RET:           INDIRECT(RET);            break;
    case REV:           INDIRECT(REV);            break;
    case ROUNDSN:       INDIRECT(ROUNDSN);        break;
    case RUNP:          INDIRECT(RUNP);           break;
    case SAVEH:         INDIRECT(SAVEH);          break;
    case SAVEL:         INDIRECT(SAVEL);          break;
    case SB:            INDIRECT(SB);             break;
    case SETERR:        INDIRECT(SETERR);         break;
    case SETHALTERR:    INDIRECT(SETHALTERR);     break;
    case SHL:           INDIRECT(SHL);            break;
    case SHR:           INDIRECT(SHR);            break;
    case STARTP:        INDIRECT(STARTP);         break;
    case STHB:          INDIRECT(STHB);           break;
    case STHF:          INDIRECT(STHF);           break;
    case STLB:          INDIRECT(STLB);           break;
    case STLF:          INDIRECT(STLF);           break;
    case STOPERR:       INDIRECT(STOPERR);        break;
    case STOPP:         INDIRECT(STOPP);          break;
    case STTIMER:       INDIRECT(STTIMER);        break;
    case SUB:           INDIRECT(SUB);            break;
    case SUM:           INDIRECT(SUM);            break;
    case TALT:          INDIRECT(TALT);           break;
    case TALTWT:        INDIRECT(TALTWT);         break;
    case TESTERR:       INDIRECT(TESTERR);        break;
    case TESTHALTERR:   INDIRECT(TESTHALTERR);    break;
    case TESTPRANAL:    INDIRECT(TESTPRANAL);     break;
    case TIN:           INDIRECT(TIN);            break;
    case UNPACKSN:      INDIRECT(UNPACKSN);       break;
    case WCNT:          INDIRECT(WCNT);           break;
    case WSUB:          INDIRECT(WSUB);           break;
    case WSUBDB:        INDIRECT(WSUBDB);         break;
    case XDBLE:         INDIRECT(XDBLE);          break;
    case XOR:           INDIRECT(XOR);            break;
    case XWORD:         INDIRECT(XWORD);          break;

    // META operations.
    case PRINTDEC:      INDIRECT(PRINTDEC);       break;
    case PRINTHEX:      INDIRECT(PRINTHEX);       break;
    case PUTC:          INDIRECT(PUTC);           break;

    default:
      throw logic_error("Unrecognised indirect instruction: " + ::toString(op) +
                        "(" + to_string(op) + ")");
  }
}

void State::performUnit(Unit op) UNIMPLEMENTED_FP;

void State::setError() {
  Error = true;
  if (HaltOnError)
    throw runtime_error("An error occurred and HaltOnError is set.");
}

void State::deschedule() {
  // Descheduling simply requires us to not reschedule the process.
  resumeNext();
}

void State::schedule(int32_t desc) {
  int32_t new_priority = desc & 0x3;
  int32_t new_Wptr = desc & ~0x3;

  // Set the next pointer of the new process.
  write(new_Wptr - 8, NotProcess);
  enqueueProcess(desc);
}

void State::schedule() {
  write(Wptr - 4, Iptr);        // Save the instruction pointer.
  write(Wptr - 8, NotProcess);  // This process is at the back of the queue.
  schedule(Wdesc());
}

void State::resumeNext() {
  int32_t next_Wdesc;
  if (FptrReg[0] != NotProcess) {
    next_Wdesc = dequeueProcess(0);
  } else if (FptrReg[1] != NotProcess) {
    next_Wdesc = dequeueProcess(1);
  } else {
    throw runtime_error("All processes have stopped.");
  }
  Wptr = next_Wdesc & ~0x3;
  Iptr = read(Wptr - 4);
}

void State::yield() {
  schedule();
  resumeNext();
}

void State::stop() {
  deschedule();
}

int32_t State::time() {
  return ClockReg[priority];
}

int32_t State::Wdesc() {
  return Wptr | priority;
}

bool State::channelReady(int32_t address) {
  UNIMPLEMENTED("channelReady");
}

bool State::isExternalChannel(int32_t address) {
  switch (address) {
    // These are the memory-mapped addresses of the external channels.
    case 0x80000000: case 0x80000004: case 0x80000008: case 0x8000000C:
    case 0x80000010: case 0x80000014: case 0x80000018: case 0x8000001C:
      return true;
    default:
      return false;
  }
}

bool State::after(int32_t a, int32_t b) {
  uint32_t offset = static_cast<uint32_t>(a) - static_cast<uint32_t>(b);
  return (offset < 0x80000000);
}
