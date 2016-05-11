#include "Instance.h"

Instance::Instance(const InstanceDescriptor& descriptor,
                   const char* bytecode, int32_t bytecode_size,
                   const int32_t* static_data, int32_t static_data_size)
    : VM(descriptor.workspace_pointer - descriptor.bytes_needed + 4,
         descriptor.bytes_needed, static_data, static_data_size, bytecode,
         bytecode_size) {
  set_workspace_pointer(descriptor.workspace_pointer);
  set_instruction_pointer(descriptor.instruction_pointer);
}
