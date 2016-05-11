#include "network.h"

#define DEFINE_MESSAGE(name)  \
  template <> Network MessageNameChecker<name>::type = name

// START_PROCESS_SERVER

DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  message->name = readString();
  message->description = readString();
  message->data = readString();
  message->data_start = readInt32();
  message->bytecode = readString();
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.name);
  writeString(message.description);
  writeString(message.data);
  writeInt32(message.data_start);
  writeString(message.bytecode);
}

// InstanceDescriptor

template <>
void BinaryReader::read(InstanceDescriptor* descriptor) {
  // Workspace pointer is likely to be very negative.
  descriptor->workspace_pointer = readInt32();
  descriptor->instruction_pointer = readVarInt();
  descriptor->space_needed = readVarUint();
}

template <>
void BinaryWriter::write(const InstanceDescriptor& descriptor) {
  writeInt32(descriptor.workspace_pointer);
  writeVarInt(descriptor.instruction_pointer);
  writeVarUint(descriptor.space_needed);
}

// REQUEST_INSTANCE

DEFINE_MESSAGE(REQUEST_INSTANCE);

READER(REQUEST_INSTANCE) {
  message->parent_id = readVarUint();
  read(&message->descriptor);
}

WRITER(REQUEST_INSTANCE) {
  writeVarUint(message.parent_id);
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

// INSTANCE_EXITED

DEFINE_MESSAGE(INSTANCE_EXITED);

READER(INSTANCE_EXITED) {
  message->id = readVarUint();
}

WRITER(INSTANCE_EXITED) {
  writeVarUint(message.id);
}
