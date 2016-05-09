#include "Instance.h"

Instance::Instance(int32_t memory_start, int32_t memory_size,
                   const char* bytecode, int32_t bytecode_size,
                   int32_t data_start, const int32_t* data, int32_t data_end)
    : VM(memory_start, memory_size, bytecode, bytecode_size),
      data_start_(data_start), data_end_(data_end), data_(data) {}

int32_t Instance::readBeforeStart(int32_t address) {
  if (address < data_start_ || data_end_ <= address)
    return VM::readBeforeStart(address);
  int32_t index =
      static_cast<int32_t>(static_cast<uint32_t>(address - data_start_) >> 2);
  return data_[index];
}
