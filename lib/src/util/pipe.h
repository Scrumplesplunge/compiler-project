#include "stream.h"

#include <condition_variable>
#include <mutex>

// Synchronous cross-thread pipe. Simultaneous access is allowed for exactly one
// reader and one writer.
class Pipe : public InputStream, public OutputStream {
 public:
  Pipe() = default;

  // Reading.
  bool get(char& c) override;
  int64_t read(char* buffer, int64_t length) override;
  void closeInput() override;

  // Writing.
  bool put(char c) override;
  int64_t write(const char* buffer, int64_t length) override;
  void closeOutput() override;

 private:
  // Signal to a waiting reader that it is finished.
  void readDone();

  // Wait until a read is completed by a sequence of writes, or until the pipe
  // is closed for writing.
  void readWait(std::unique_lock<std::mutex>& lock);

  // Signal to a waiting writer that it is finished.
  void writeDone();
  
  // Wait until a write is consumed by a sequence of reads.
  void writeWait(std::unique_lock<std::mutex>& lock);

  std::mutex mu_;
  std::condition_variable on_read_event_, on_write_event_;

  bool waiting_ = false;
  bool input_closed_ = false;   // True if no more bytes will be read.
  bool output_closed_ = false;  // True if no more bytes will be written.

  char* reader_buffer_ = nullptr;
  int64_t reader_length_ = 0;

  const char* writer_buffer_;
  int64_t writer_length_ = 0;
};
