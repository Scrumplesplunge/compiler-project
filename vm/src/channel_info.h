#pragma once

#include "network.h"

#include <mutex>
#include <string>
#include <unordered_map>

struct WaitingReader {
  instance_id id;
};

struct WaitingWriter {
  instance_id id;
  std::string data;
};

struct ChannelHasher {
  size_t operator()(const Channel& channel) const;
};

struct ChannelInfo {
  std::mutex mutex;

  std::unordered_map<Channel, WaitingReader, ChannelHasher> enabled;
  std::unordered_map<Channel, WaitingReader, ChannelHasher> readers;
  std::unordered_map<Channel, WaitingWriter, ChannelHasher> writers;
};
