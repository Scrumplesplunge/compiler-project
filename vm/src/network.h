#pragma once

#include "Network.h"

#include <stdint.h>
#include <string>
#include <util/binary.h>

#define READER(name)  \
  template <> void BinaryReader::read(name##_Message* message)
#define WRITER(name)  \
  template <> void BinaryWriter::write(const name##_Message& message)
#define DECLARE_MESSAGE(name)  \
  struct name##_Message;  \
  READER(name);  \
  WRITER(name);  \
  struct name##_Message : public MessageNameChecker<name>
#define MESSAGE(name)  \
  name##_Message
#define ON(name, handler)  \
  on<MESSAGE(name)>(name, handler)

typedef uint64_t instance_id;
typedef uint64_t worker_id;

template <Network message_type>
struct MessageNameChecker {
  static Network type;
};

DECLARE_MESSAGE(START_PROCESS_SERVER) {
  // This information is shown in the verbose output on the process server.
  std::string name;
  std::string description;

  std::string data;
  int32_t data_start;
  std::string bytecode;
};

struct InstanceDescriptor {
  int32_t workspace_pointer;
  int32_t instruction_pointer;
  uint32_t space_needed;
};

template <> void BinaryReader::read(InstanceDescriptor* descriptor);
template <> void BinaryWriter::write(const InstanceDescriptor& descriptor);

DECLARE_MESSAGE(REQUEST_INSTANCE) {
  instance_id parent_id;

  InstanceDescriptor descriptor;
};

DECLARE_MESSAGE(START_INSTANCE) {
  instance_id id;

  InstanceDescriptor descriptor;
};

DECLARE_MESSAGE(INSTANCE_EXITED) {
  instance_id id;
};

#undef DECLARE_MESSAGE
