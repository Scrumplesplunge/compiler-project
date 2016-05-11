#include "network.h"

#define DEFINE_MESSAGE(name)  \
  template <> Network MessageNameChecker<name>::type = name

// START_PROCESS_SERVER

DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  message->name = readString();
  message->description = readString();
  message->data = readString();
  message->bytecode = readString();
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.name);
  writeString(message.description);
  writeString(message.data);
  writeString(message.bytecode);
}

// InstanceDescriptor

template <>
void BinaryReader::read(InstanceDescriptor* descriptor) {
  // Workspace pointer is likely to be very negative.
  descriptor->workspace_pointer = readInt32();
  descriptor->instruction_pointer = readVarInt();
  descriptor->bytes_needed = readVarUint();
}

template <>
void BinaryWriter::write(const InstanceDescriptor& descriptor) {
  writeInt32(descriptor.workspace_pointer);
  writeVarInt(descriptor.instruction_pointer);
  writeVarUint(descriptor.bytes_needed);
}

// REQUEST_INSTANCE

DEFINE_MESSAGE(REQUEST_INSTANCE);

READER(REQUEST_INSTANCE) {
  message->parent_id = readVarUint();
  message->parent_workspace_descriptor = readInt32();
  read(&message->descriptor);
}

WRITER(REQUEST_INSTANCE) {
  writeVarUint(message.parent_id);
  writeInt32(message.parent_workspace_descriptor);
  write(message.descriptor);
}

// START_INSTANCE

DEFINE_MESSAGE(START_INSTANCE);

READER(START_INSTANCE) {
  message->id = readVarUint();
  read(&message->descriptor);
}

WRITER(START_INSTANCE) {
  writeVarUint(message.id);
  write(message.descriptor);
}

// INSTANCE_STARTED

DEFINE_MESSAGE(INSTANCE_STARTED);

READER(INSTANCE_STARTED) {
  message->id = readVarUint();
  message->parent_id = readVarUint();
  message->parent_workspace_descriptor = readInt32();
}

WRITER(INSTANCE_STARTED) {
  writeVarUint(message.id);
  writeVarUint(message.parent_id);
  writeInt32(message.parent_workspace_descriptor);
}

// INSTANCE_EXITED

DEFINE_MESSAGE(INSTANCE_EXITED);

READER(INSTANCE_EXITED) {
  message->id = readVarUint();
}

WRITER(INSTANCE_EXITED) {
  writeVarUint(message.id);
}
