#pragma once

#include "../network.h"
#include "../runtime/VM.h"

#include <unordered_map>

class Instance : public VM {
 public:
  Instance(const InstanceDescriptor& descriptor, const char* bytecode,
           int32_t bytecode_size, const int32_t* static_data,
           int32_t static_data_size);
  virtual ~Instance() = default;
};
