#pragma once

#include "../channel_info.h"
#include "../network.h"
#include "../runtime/metadata.h"
#include "config.h"
#include "ProcessTree.h"

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <stdint.h>
#include <unordered_map>
#include <util/args.h>
#include <util/atomic_output.h>
#include <util/messenger.h>
#include <util/socket.h>
#include <vector>

class ProcessMaster;

class ProcessServerHandle {
 public:
  ProcessServerHandle(
      const std::string& job_name, const std::string& job_description,
      ProcessMaster& master, worker_id id, Socket&& socket, std::string data,
      std::string bytecode);

  void startInstance(instance_id id, InstanceDescriptor descriptor);
  void instanceStarted(instance_id id, instance_id parent_id,
                       int32_t parent_workspace_descriptor);

  void serve();

  void close();

  // Perform a ping and return the latency. This is synchronous. The value
  // returned is the latency in nanoseconds.
  int64_t latency();

  std::string hostPort() { return messenger_.hostPort(); }

 private:
  friend class ProcessMaster;

  void onPong(MESSAGE(PONG)&& message);

  template <typename T>
  void send(const T& message) {
    messenger_.send(message.type, message);
  }

  worker_id id_;
  ProcessMaster& master_;
  Messenger messenger_;

  std::mutex latency_mu_;
  std::condition_variable on_pong_, on_latency_complete_;
  bool ping_active_ = false;
  bool pong_back_ = false;
};

class ProcessMaster {
 public:
  ProcessMaster(const JobConfig& config);

  void serve();

 private:
  friend class ProcessServerHandle;

  void onRequestInstance(MESSAGE(REQUEST_INSTANCE)&& message);
  void onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message);
  void onChannelInput(MESSAGE(CHANNEL_IN)&& message);
  void onChannelOutput(MESSAGE(CHANNEL_OUT)&& message);
  void onChannelOutputDone(MESSAGE(CHANNEL_OUT_DONE)&& message);
  void onChannelEnable(MESSAGE(CHANNEL_ENABLE)&& message);
  void onChannelDisable(MESSAGE(CHANNEL_DISABLE)&& message);
  void onChannelReset(MESSAGE(CHANNEL_RESET)&& message);

  void forwardOutput(Channel channel, const WaitingReader& reader,
                     const WaitingWriter& writer);

  void outputDone(Channel channel, instance_id writer);

  void resolveChannelMessage(Channel channel, const WaitingReader& reader,
                             const WaitingWriter& writer);

  // Job info.
  const std::string job_name_;
  const std::string job_description_;

  // Program info.
  const std::string bytecode_;
  const MetaData metadata_;

  // Running instances.
  ProcessTree process_tree_;

  // Info about active channels.
  ChannelInfo channels_;

  // Running workers.
  std::atomic<worker_id> next_worker_to_use_{0};
  std::vector<std::unique_ptr<ProcessServerHandle>> workers_;
};
