#pragma once

#include "../runtime/VM.h"

#include <unordered_map>

class Instance : public VM {
 public:
  Instance(int32_t memory_start, int32_t memory_size,
           const char* bytecode, int32_t bytecode_size,
           int32_t data_start, const int32_t* data, int32_t data_end);
  virtual ~Instance() = default;

 protected:
  int32_t readBeforeStart(int32_t address) override;

 private:
  int32_t data_start_, data_end_;
  const int32_t* data_;
};
