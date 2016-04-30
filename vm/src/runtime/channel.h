#pragma once

#include <atomic>

class VM;

class ChannelReader {
 public:
  // Enable or disable an ALT which is waiting on this channel.
  virtual void markEnabled(int32_t workspace_descriptor) = 0;
  virtual void markDisabled() = 0;

  // Returns true if the channel is enabled.
  virtual bool is_enabled() const = 0;

  // Check whether a writer process is waiting on the channel.
  virtual bool is_ready() const = 0;

  // If the channel is not ready, wait directly on this channel.
  virtual void readWait(int32_t workspace_descriptor) = 0;

  // Must only be called during readWait. Returns the workspace descriptor of
  // the waiting reader.
  virtual int32_t workspace_descriptor() const = 0;

  // Must only be called during readWait. Returns true once the writer has
  // acted.
  virtual bool is_done() const = 0;

  // Once the channel is ready, the transaction can be performed.
  virtual void read(int32_t address, int32_t length, VM* vm) = 0;

  // Reset the channel.
  virtual void reset() = 0;
};

class ChannelWriter {
 public:
  // Check whether a reader process is waiting directly on the channel.
  virtual bool is_ready() const = 0;

  // Wait directly on this channel.
  virtual void writeWait(int32_t workspace_descriptor) = 0;

  // Must only be called during writeWait. Returns the workspace descriptor of
  // the waiting writer.
  virtual int32_t workspace_descriptor() const = 0;

  // Must only be called during writeWait. Returns true once the reader has
  // acted.
  virtual bool is_done() const = 0;

  // Once the channel is ready, the transaction can be performed.
  virtual void write(const VM& vm, int32_t address, int32_t length) = 0;

  // Reset the channel.
  virtual void reset() = 0;
};
