#include "network.h"

#define DEFINE_MESSAGE(name)  \
  template <> Network MessageNameChecker<name>::type = name

// START_PROCESS_SERVER

DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  MESSAGE(START_PROCESS_SERVER) message;
  message.data = readString();
  message.bytecode = readString();
  return message;
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.data);
  writeString(message.bytecode);
}
