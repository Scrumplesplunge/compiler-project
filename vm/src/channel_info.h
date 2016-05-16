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

struct WaitingProcess {
  instance_id id;
  int32_t workspace_descriptor;
};

struct ChannelHasher {
  size_t operator()(const Channel& channel) const;
};

struct ChannelInfo {
  std::mutex mutex;

  std::unordered_map<Channel, WaitingReader, ChannelHasher> enabled;
  std::unordered_map<Channel, WaitingReader, ChannelHasher> readers;
  std::unordered_map<Channel, WaitingWriter, ChannelHasher> writers;
  std::unordered_map<Channel, WaitingProcess, ChannelHasher> processes;
};
