#pragma once

#include "../network.h"
#include "Instance.h"
#include "PartialProcessTree.h"

#include <memory>
#include <mutex>
#include <stdint.h>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <util/args.h>
#include <util/atomic_output.h>
#include <util/messenger.h>
#include <util/socket.h>

class ProcessServer {
 public:
  ProcessServer(Socket&& socket);

  void serve();

 private:
  friend class Instance;

  // Called by an instance which then passes itself and suspends the
  // corresponding process. The process will be rescheduled when the instance ID
  // has been returned.
  void requestInstance(InstanceDescriptor descriptor, instance_id parent,
                       int32_t workspace_descriptor);

  void notifyExited(instance_id id);

  void joinInstance(instance_id join_id, instance_id waiter_id,
                    int32_t workspace_descriptor);

  template <typename T>
  void send(const T& message) {
    messenger_.send(message.type, message);
  }

  void onStartProcessServer(MESSAGE(START_PROCESS_SERVER)&& message);
  void onStartInstance(MESSAGE(START_INSTANCE)&& message);
  void onInstanceStarted(MESSAGE(INSTANCE_STARTED)&& message);
  void onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message);

  void onPing(MESSAGE(PING)&& message);

  worker_id id_;

  std::mutex instance_mu_;  // Guards instances_ and instance_threads_.
  std::unordered_map<instance_id, std::unique_ptr<Instance>> instances_;
  std::unordered_map<instance_id, std::thread> instance_threads_;

  PartialProcessTree process_tree_;

  bool is_ready_;

  int32_t data_start_, data_end_;
  std::unique_ptr<int32_t[]> data_;

  std::string bytecode_;

  struct WaitingProcess {
    instance_id id;
    int32_t workspace_descriptor;
  };

  // At most one process can wait for another to exit, and this should be its
  // parent.
  std::mutex exit_mu_;  // Guards unjoined_exits_ and on_exited_.
  std::unordered_set<instance_id> unjoined_exits_;
  std::unordered_map<instance_id, WaitingProcess> on_exited_;

  Messenger messenger_;
};
