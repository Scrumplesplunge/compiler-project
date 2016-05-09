#include "network.h"

#define DEFINE_MESSAGE(name)  \
  template <> Network MessageNameChecker<name>::type = name

// START_PROCESS_SERVER

DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  message->data = move(readString());
  message->data_start = readInt32();
  message->bytecode = move(readString());
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.data);
  writeInt32(message.data_start);
  writeString(message.bytecode);
}

// REQUEST_INSTANCE

DEFINE_MESSAGE(REQUEST_INSTANCE);

READER(REQUEST_INSTANCE) {
  // Workspace pointer is likely to be very negative.
  message->workspace_pointer = readInt32();
  message->instruction_pointer = readVarInt();
  message->space_needed = readVarUint();
}

WRITER(REQUEST_INSTANCE) {
  writeInt32(message.workspace_pointer);
  writeVarInt(message.instruction_pointer);
  writeVarUint(message.space_needed);
}

// START_INSTANCE

DEFINE_MESSAGE(START_INSTANCE);

READER(START_INSTANCE) {
  read(&message->request);
  message->instance_id = readVarUint();
}

WRITER(START_INSTANCE) {
  write(message.request);
  writeVarUint(message.instance_id);
}
