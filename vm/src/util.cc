#include "util.h"

#include <fstream>

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

string getFileContents(string filename) {
  ifstream file(filename, ios::binary);
  if (!file)
    throw runtime_error("Could not open \"" + filename + "\" for reading.");
  return string(istreambuf_iterator<char>(file), istreambuf_iterator<char>());
}

string units(int64_t value, string unit) {
  return to_string(value) + " " + unit + (value == 1 ? "" : "s");
}

// Format a duration as a string.
string formatDuration(int64_t ns) {
  int64_t us = ns / 1000; ns %= 1000;
  int64_t ms = us / 1000; us %= 1000;
  int64_t s = ms / 1000;  ms %= 1000;
  int64_t m = s / 60;     s %= 60;
  int64_t h = m / 60;     m %= 60;

  if (h > 0)
    return units(h, "hour") + " and " + units(m, "minute");
  if (m > 0)
    return units(m, "minute") + " and " + units(s, "second");
  if (s > 0)
    return units(s, "second") + " and " + units(ms, "millisecond");
  if (ms > 0)
    return units(ms, "millisecond") + " and " + units(us, "microsecond");
  if (us > 0)
    return units(us, "microsecond") + " and " + units(ns, "nanosecond");
  if (ns > 0)
    return units(ns, "nanosecond");
  return "0 seconds";
}
