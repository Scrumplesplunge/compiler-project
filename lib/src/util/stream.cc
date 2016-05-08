#include "stream.h"

#include <string.h>

using namespace std;

int64_t InputStream::read(char* buffer, int64_t length) {
  for (int64_t i = 0; i < length; i++)
    if (!get(buffer[i])) return i;
  return length;
}

int64_t OutputStream::write(const char* buffer, int64_t length) {
  for (int64_t i = 0; i < length; i++)
    if (!put(buffer[i])) return i;
  return length;
}

StandardInputStream::StandardInputStream(istream& input)
    : input_(input) {}

bool StandardInputStream::get(char& c) {
  input_.get(c);
  return input_.good();
}

int64_t StandardInputStream::read(char* buffer, int64_t length) {
  input_.read(buffer, length);
  return input_.gcount();
}

void StandardInputStream::closeInput() {
  // This function has no effect for standard streams.
}

StandardOutputStream::StandardOutputStream(ostream& output)
    : output_(output) {}

bool StandardOutputStream::put(char c) {
  output_.put(c);
  return output_.good();
}

int64_t StandardOutputStream::write(const char* buffer, int64_t length) {
  int64_t start = output_.tellp();
  output_.write(buffer, length);
  if (output_.good()) return length;

  // Not all characters were written successfully.

  // If the number of characters written cannot be determined, return a negative
  // number.
  if (start == -1) return -1;

  // Otherwise, compute how many were written.
  int64_t end = output_.tellp();
  return end - start;
}

void StandardOutputStream::closeOutput() {
  // This function has no effect for standard streams.
}

namespace stream {
  StandardInputStream in(cin);

  StandardOutputStream out(cout);
  StandardOutputStream error(cerr);
}
