#pragma once

#include "Network.h"

#include <list>
#include <stdint.h>
#include <string>
#include <util/binary.h>

// Auxiliary types.
typedef uint64_t instance_id;
typedef uint64_t worker_id;

// Describes the initial configuration of an instance.
struct InstanceDescriptor {
  int32_t workspace_pointer;
  int32_t instruction_pointer;
  uint32_t bytes_needed;
};

template <> void BinaryReader::read(InstanceDescriptor* descriptor);
template <> void BinaryWriter::write(const InstanceDescriptor& descriptor);

// Stores metadata about an instance.
struct InstanceInfo {
  InstanceInfo() = default;
  InstanceInfo(worker_id worker, const InstanceDescriptor& descriptor);

  // Tree information.
  instance_id id, parent_id;

  // Location where this instance is hosted.
  worker_id location;

  // Bounds of the memory associated with this instance. The start bound is
  // inclusive, and the end bound is exclusive.
  int32_t memory_start, memory_end;
};

template <> void BinaryReader::read(InstanceInfo* info);
template <> void BinaryWriter::write(const InstanceInfo& info);

void readDelta(BinaryReader* reader, InstanceInfo* info);
void writeDelta(BinaryWriter* writer, const InstanceInfo& previous,
                const InstanceInfo& info);

// Stores a list of instances such that each element (except the first) is a
// direct child of the one before it.
struct Ancestry {
  // The instance whose ancestry this is.
  instance_id subject;

  // Ordered list of ancestors. Each element is a direct child of the previous
  // one, except for the first element. The instance identified by subject is
  // not included, but is a direct child of the last entry in this list. The
  // list may be empty.
  std::list<InstanceInfo> contents;
};

template <> void BinaryReader::read(Ancestry* ancestry);
template <> void BinaryWriter::write(const Ancestry& ancestry);

// Uniquely identifies a channel.
struct Channel {
  bool operator==(const Channel& other) const {
    return owner == other.owner && address == other.address;
  }

  instance_id owner;
  int32_t address;
};

template <> void BinaryReader::read(Channel* channel);
template <> void BinaryWriter::write(const Channel& channel);

// Base class for channel events.
struct ChannelEvent {
  Channel channel;
  instance_id actor;
};

template <> void BinaryReader::read(ChannelEvent* channel);
template <> void BinaryWriter::write(const ChannelEvent& channel);

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

template <Network message_type>
struct MessageNameChecker {
  static Network type;
};

DECLARE_MESSAGE(START_PROCESS_SERVER) {
  // This information is shown in the verbose output on the process server.
  std::string name;
  std::string description;

  std::string data;
  std::string bytecode;

  worker_id id;
};

DECLARE_MESSAGE(REQUEST_INSTANCE) {
  instance_id parent_id;
  int32_t parent_workspace_descriptor;

  InstanceDescriptor descriptor;
};

DECLARE_MESSAGE(START_INSTANCE) {
  instance_id id;

  InstanceDescriptor descriptor;
  Ancestry ancestry;
};

DECLARE_MESSAGE(INSTANCE_STARTED) {
  instance_id id, parent_id;
  int32_t parent_workspace_descriptor;
};

DECLARE_MESSAGE(INSTANCE_EXITED) {
  instance_id id;
};

DECLARE_MESSAGE(CHANNEL_IN), public ChannelEvent {};

DECLARE_MESSAGE(CHANNEL_OUT), public ChannelEvent {
  std::string data;
};

DECLARE_MESSAGE(CHANNEL_OUT_DONE), public ChannelEvent {};
DECLARE_MESSAGE(CHANNEL_ENABLE), public ChannelEvent {};
DECLARE_MESSAGE(CHANNEL_DISABLE), public ChannelEvent {};
DECLARE_MESSAGE(CHANNEL_RESET), public ChannelEvent {};

DECLARE_MESSAGE(PING) {};
DECLARE_MESSAGE(PONG) {};

#undef DECLARE_MESSAGE
