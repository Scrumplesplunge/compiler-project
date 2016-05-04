#include "util.h"

using namespace std;

char hex(int8_t value) {
  const char digits[] = "0123456789abcdef";
  return digits[value];
}

string addressString(int32_t address) {
  uint32_t raw = static_cast<uint32_t>(address);
  char out[8];
  for (int i = 8; i-- > 0;) {
    out[i] = hex(raw & 0xF);
    raw >>= 4;
  }
  return "0x" + string(out, 8);
}
