#pragma once

#include "../network.h"
#include "master/ChannelMasterStateType.h"

#include <mutex>
#include <unordered_map>

class ProcessMaster;

class ChannelMaster {
 public:
  ChannelMaster(ProcessMaster& master)
      : master_(master) {}

  void onInput(worker_id worker, MESSAGE(CHANNEL_INPUT)&& message);
  void onOutput(worker_id worker, MESSAGE(CHANNEL_OUTPUT)&& message);
  void onEnable(worker_id worker, MESSAGE(CHANNEL_ENABLE)&& message);
  void onDisable(worker_id worker, MESSAGE(CHANNEL_DISABLE)&& message);
  void onResolved(worker_id worker, MESSAGE(CHANNEL_RESOLVED)&& message);
  void onDone(worker_id worker, MESSAGE(CHANNEL_DONE)&& message);

 private:
  struct ChannelState;

  ChannelState& get(Channel channel);

  struct Enabler {
    worker_id worker;
  };

  struct Reader {
    worker_id worker;
  };

  struct Writer {
    worker_id worker;
    std::string data;
  };

  struct ChannelState {
    ChannelMasterStateType type = NORMAL;

    worker_id owner_worker;

    Enabler enabler;
    Reader reader;
    Writer writer;
  };

  std::mutex mu_;

  ProcessMaster& master_;
  std::unordered_map<Channel, ChannelState, ChannelHasher> channels_;
};
