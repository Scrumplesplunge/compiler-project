#include "network.h"

#include "util.h"

#include <util/atomic_output.h>

using namespace std;

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

// InstanceInfo
InstanceInfo::InstanceInfo(
    worker_id worker, const InstanceDescriptor& descriptor) {
  location = worker;
  memory_end = descriptor.workspace_pointer + 4;
  memory_start = memory_end - descriptor.bytes_needed;
}

template <>
void BinaryReader::read(InstanceInfo* info) {
  info->parent_id = readVarUint();

  // ID is encoded as delta from parent ID.
  info->id = info->parent_id + readVarUint();

  info->location = readVarUint();
  info->memory_start = readInt32();
  info->memory_end = readInt32();
}

template <>
void BinaryWriter::write(const InstanceInfo& info) {
  writeVarUint(info.parent_id);

  // ID is encoded as delta from parent ID.
  writeVarUint(info.id - info.parent_id);

  writeVarUint(info.location);
  writeInt32(info.memory_start);
  writeInt32(info.memory_end);
}

void readDelta(BinaryReader* reader, InstanceInfo* info) {
  info->parent_id = info->id;
  info->id += reader->readVarUint();
  info->location = reader->readVarUint();
  info->memory_start += reader->readVarInt();
  info->memory_end += reader->readVarInt();
}

void writeDelta(BinaryWriter* writer, const InstanceInfo& previous,
                const InstanceInfo& info) {
  writer->writeVarUint(info.id - previous.id);
  writer->writeVarUint(info.location);
  writer->writeVarInt(info.memory_start - previous.memory_start);
  writer->writeVarInt(info.memory_end - previous.memory_end);
}

// Ancestry
template <>
void BinaryReader::read(Ancestry* ancestry) {
  ancestry->subject = readVarUint();
  ancestry->contents.clear();

  uint64_t num_entries = readVarUint();
  if (num_entries == 0) return;

  // Read the first entry.
  InstanceInfo info;
  read(&info);
  ancestry->contents.push_front(info);

  for (uint64_t i = 1; i < num_entries; i++) {
    // Read each (delta encoded) entry.
    readDelta(this, &info);
    ancestry->contents.push_front(info);
  }
}

template <>
void BinaryWriter::write(const Ancestry& ancestry) {
  writeVarUint(ancestry.subject);
  writeVarUint(ancestry.contents.size());
  if (ancestry.contents.size() == 0) return;

  bool first = true;
  InstanceInfo info;
  for (auto i = ancestry.contents.rbegin();
       i != ancestry.contents.rend(); i++) {
    const InstanceInfo& instance = *i;
    if (first) {
      // Write the first entry. This is not delta encoded.
      write(instance);
      first = false;
    } else {
      // Write each (delta encoded) entry.
      writeDelta(this, info, instance);
    }
    info = instance;
  }
}

// Channel
template <>
void BinaryReader::read(Channel* channel) {
  channel->owner = readVarUint();
  channel->address = readInt32();
  verr << "READ CHANNEL: (" << channel->owner << ", "
       << addressString(channel->address) << ")\n";
};

template <>
void BinaryWriter::write(const Channel& channel) {
  verr << "WRITE CHANNEL: (" << channel.owner << ", "
       << addressString(channel.address) << ")\n";
  writeVarUint(channel.owner);
  writeInt32(channel.address);
}

size_t ChannelHasher::operator()(const Channel& channel) const {
  return channel.owner ^ channel.address;
}

#define DEFINE_MESSAGE(name)  \
  template <> Network MessageNameChecker<name>::type = name

// START_PROCESS_SERVER
DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  message->name = readString();
  message->description = readString();
  message->data = readString();
  message->bytecode = readString();
  message->id = readVarUint();
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.name);
  writeString(message.description);
  writeString(message.data);
  writeString(message.bytecode);
  writeVarUint(message.id);
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
  read(&message->ancestry);
}

WRITER(START_INSTANCE) {
  writeVarUint(message.id);
  write(message.descriptor);
  write(message.ancestry);
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
  verr << "READ INSTANCE_EXITED: " << message->id << "\n";
}

WRITER(INSTANCE_EXITED) {
  verr << "WRITE INSTANCE_EXITED: " << message.id << "\n";
  writeVarUint(message.id);
}

// CHANNEL_OUTPUT
DEFINE_MESSAGE(CHANNEL_OUTPUT);

READER(CHANNEL_OUTPUT) {
  read(&message->channel);
  message->data = readString();
}

WRITER(CHANNEL_OUTPUT) {
  write(message.channel);
  writeString(message.data);
}

// CHANNEL_INPUT
DEFINE_MESSAGE(CHANNEL_INPUT);

READER(CHANNEL_INPUT) {
  read(&message->channel);
}

WRITER(CHANNEL_INPUT) {
  write(message.channel);
}

// CHANNEL_ENABLE
DEFINE_MESSAGE(CHANNEL_ENABLE);

READER(CHANNEL_ENABLE) {
  read(&message->channel);
}

WRITER(CHANNEL_ENABLE) {
  write(message.channel);
}

// CHANNEL_DISABLE
DEFINE_MESSAGE(CHANNEL_DISABLE);

READER(CHANNEL_DISABLE) {
  read(&message->channel);
}

WRITER(CHANNEL_DISABLE) {
  write(message.channel);
}

// CHANNEL_RESOLVED
DEFINE_MESSAGE(CHANNEL_RESOLVED);

READER(CHANNEL_RESOLVED) {
  read(&message->channel);
}

WRITER(CHANNEL_RESOLVED) {
  write(message.channel);
}

// CHANNEL_DONE
DEFINE_MESSAGE(CHANNEL_DONE);

READER(CHANNEL_DONE) {
  read(&message->channel);
}

WRITER(CHANNEL_DONE) {
  write(message.channel);
}

// PING
DEFINE_MESSAGE(PING);

READER(PING) {}
WRITER(PING) {}

// PONG
DEFINE_MESSAGE(PONG);

READER(PONG) {}
WRITER(PONG) {}
