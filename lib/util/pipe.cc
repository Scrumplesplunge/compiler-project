#include "pipe.h"

#include <string.h>

using namespace std;

bool Pipe::get(char& c) {
  unique_lock<mutex> lock(mu_);
  if (output_closed_) return false;

  if (waiting_) {
    // Writer is waiting. Consume a character.
    c = writer_buffer_[0];
    writer_buffer_++;
    writer_length_--;
    if (writer_length_ == 0) writeDone();
    return true;
  } else {
    reader_buffer_ = &c;
    reader_length_ = 1;
    readWait(lock);
    return reader_length_ == 0;
  }
}

int64_t Pipe::read(char* buffer, int64_t length) {
  unique_lock<mutex> lock(mu_);
  if (output_closed_) return 0;

  if (waiting_) {
    if (writer_length_ >= length) {
      // Reader can terminate without waiting.
      memcpy(buffer, writer_buffer_, length);
      writer_buffer_ += length;
      writer_length_ -= length;
      if (writer_length_ == 0) writeDone();
      return length;
    } else {
      // Reader will have to wait for more data. Transfer as much as possible
      // and then wait for more writes to finish this read.
      memcpy(buffer, writer_buffer_, writer_length_);
      reader_buffer_ = buffer + writer_length_;
      reader_length_ = length - writer_length_;
      writer_length_ = 0;
      writeDone();
      readWait(lock);
      return length - reader_length_;
    }
  } else {
    // No writer is waiting. Wait for writers to finish this read.
    reader_buffer_ = buffer;
    reader_length_ = length;
    readWait(lock);
    return length - reader_length_;
  }
}

void Pipe::closeInput() {
  // Close for reading and signal to any waiting writers.
  unique_lock<mutex> lock(mu_);
  input_closed_ = true;
  on_write_event_.notify_one();
}

bool Pipe::put(char c) {
  unique_lock<mutex> lock(mu_);
  if (input_closed_) return false;

  if (waiting_) {
    // Reader is waiting. Consume a character.
    reader_buffer_[0] = c;
    reader_length_--;
    if (reader_length_ == 0) readDone();
    return true;
  } else {
    writer_buffer_ = &c;
    writer_length_ = 1;
    writeWait(lock);
    return writer_length_ == 0;
  }
}

int64_t Pipe::write(const char* buffer, int64_t length) {
  unique_lock<mutex> lock(mu_);
  if (input_closed_) return 0;

  if (waiting_) {
    if (reader_length_ >= length) {
      // Writer can terminate without waiting.
      memcpy(reader_buffer_, buffer, length);
      reader_buffer_ += length;
      reader_length_ -= length;
      if (reader_length_ == 0) readDone();
      return length;
    } else {
      // Reader will have to wait for more data. Transfer as much as possible
      // and then wait for more reads to finish this write.
      memcpy(reader_buffer_, buffer, reader_length_);
      writer_buffer_ = buffer + reader_length_;
      writer_length_ = length - reader_length_;
      reader_length_ = 0;
      readDone();
      writeWait(lock);
      return length - writer_length_;
    }
  } else {
    // No writer is waiting. Wait for writers to finish this read.
    writer_buffer_ = buffer;
    writer_length_ = length;
    writeWait(lock);
    return length - writer_length_;
  }
}

void Pipe::closeOutput() { 
  // Close for writing and signal to any waiting readers.
  unique_lock<mutex> lock(mu_);
  output_closed_ = true;
  on_read_event_.notify_one();
}

void Pipe::readDone() {
  waiting_ = false;
  on_read_event_.notify_one();
}

void Pipe::readWait(unique_lock<mutex>& lock) {
  // Wait until writers finish the read.
  waiting_ = true;
  while (!output_closed_ && reader_length_ > 0) on_read_event_.wait(lock);
}

void Pipe::writeDone() {
  waiting_ = false;
  on_write_event_.notify_one();
}

void Pipe::writeWait(unique_lock<mutex>& lock) {
  // Wait until readers consume the write.
  waiting_ = true;
  while (!output_closed_ && reader_length_ > 0) on_write_event_.wait(lock);
}
