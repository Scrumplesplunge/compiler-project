#pragma once

#include <iostream>
#include <stdint.h>

class InputStream {
 public:
  // Reads a single character and returns true if successful.
  virtual bool get(char& c) = 0;

  // Reads a sequence of characters of the at most the specified length. If the
  // stream terminates before the length limit is reached, then the return value
  // indicates how many characters were read. Otherwise, the return value is
  // equal to the length.
  virtual int64_t read(char* buffer, int64_t length);

  // Close the stream, indicating that no more characters will be read.
  virtual void closeInput() = 0;
};

class OutputStream {
 public:
  // Writes a single character and returns true if successful.
  virtual bool put(char c) = 0;

  // Writes a sequence of characters of at most the specified length. If the
  // return value is not equal to the length, then the stream was closed before
  // the write completed and the return value indicates how many characters were
  // written before this happened. If this cannot be determined, then the
  // return value will be negative.
  virtual int64_t write(const char* buffer, int64_t length);

  // Close the stream, indicating that no more characters will be written.
  virtual void closeOutput() = 0;
};

// Wrapper classes for standard streams.

class StandardInputStream : public InputStream {
 public:
  StandardInputStream(std::istream& input);

  bool get(char& c) override;
  int64_t read(char* buffer, int64_t length) override;
  void closeInput() override;

 private:
  std::istream& input_;
};

class StandardOutputStream : public OutputStream {
 public:
  StandardOutputStream(std::ostream& output);

  bool put(char c) override;
  int64_t write(const char* buffer, int64_t length) override;
  void closeOutput() override;

 private:
  std::ostream& output_;
};

// Standard streams.
namespace stream {
  extern StandardInputStream in;

  extern StandardOutputStream out;
  extern StandardOutputStream error;
}
