#include "atomic_output.h"

#include <chrono>
#include <stdio.h>

using namespace std;
using namespace std::chrono;

AtomicOutput::AtomicOutput(ostream& output)
    : output_(output) {}

BufferedOutput::BufferedOutput(AtomicOutput& output)
    : output_(output) {}

BufferedOutput::~BufferedOutput() {
  output_ << buffer_.str();
}

LoggingOutput::LoggingOutput(AtomicOutput& output)
    : BufferedOutput(output) {
  static const high_resolution_clock::time_point start =
      high_resolution_clock::now();
  high_resolution_clock::time_point now = high_resolution_clock::now();
  double time = duration<double>(now - start).count();

  char temp[10];
  snprintf(temp, 10, "[%7.2f]", time);

  (*this) << temp << " ";
}

AtomicOutput atomic_cout(cout);
AtomicOutput atomic_cerr(cerr);
