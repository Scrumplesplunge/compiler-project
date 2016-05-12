#pragma once

#include "args.h"

#include <iostream>
#include <mutex>
#include <sstream>

class AtomicOutput {
 public:
  AtomicOutput(std::ostream& output);

  template <typename T>
  AtomicOutput& operator<<(const T& value) {
    std::unique_lock<std::mutex> lock(mu_);
    output_ << value;
    return *this;
  }

 private:
  std::mutex mu_;
  std::ostream& output_;
};

class BufferedOutput {
 public:
  BufferedOutput(AtomicOutput& output);
  ~BufferedOutput();

  template <typename T>
  BufferedOutput& operator<<(const T& value) {
    buffer_ << value;
    return *this;
  }

 private:
  std::stringstream buffer_;
  AtomicOutput& output_;
};

class LoggingOutput : public BufferedOutput {
 public:
  LoggingOutput(AtomicOutput& output);
};

extern AtomicOutput atomic_cout, atomic_cerr;

#define vout  \
  if (options::verbose) LoggingOutput(atomic_cout)

#define verr  \
  if (options::verbose) LoggingOutput(atomic_cerr)
