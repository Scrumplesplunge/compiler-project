#pragma once

#include "Network.h"

#include <stdint.h>
#include <string>
#include <util/binary.h>

#define READER(name)  \
  template <> name##_Message BinaryReader::read<name##_Message>()
#define WRITER(name)  \
  template <> void BinaryWriter::write<name##_Message>(name##_Message message)

template <Network message_type>
struct MessageNameChecker {
  static Network type;
};

#define DECLARE_MESSAGE(name)  \
  struct name##_Message;  \
  READER(name);  \
  WRITER(name);  \
  struct name##_Message : public MessageNameChecker<name>

DECLARE_MESSAGE(START_PROCESS_SERVER) {
  std::string data;
  std::string bytecode;
};

#undef DECLARE_MESSAGE

#define MESSAGE(name)  \
  name##_Message

// Utilities for configuring the Messenger.
#define ON(name, handler)  \
  on<MESSAGE(name)>(name, handler)
#define SEND(message)  \
  send(message.type, message)
