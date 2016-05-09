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

template <Network message_type>
struct MessageNameChecker {
  static Network type;
};

DECLARE_MESSAGE(START_PROCESS_SERVER) {
  std::string data;
  int32_t data_start;
  std::string bytecode;
};

DECLARE_MESSAGE(REQUEST_INSTANCE) {
  int32_t workspace_pointer;
  int32_t instruction_pointer;
  int32_t space_needed;
};

DECLARE_MESSAGE(START_INSTANCE) {
  MESSAGE(REQUEST_INSTANCE) request;
  int64_t instance_id;
};

#undef DECLARE_MESSAGE
