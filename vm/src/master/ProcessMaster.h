#pragma once

#include "../network.h"
#include "../runtime/metadata.h"
#include "config.h"
#include "ProcessTree.h"

#include <condition_variable>
#include <memory>
#include <mutex>
#include <stdint.h>
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

  void startInstance(instance_id id, InstanceDescriptor&& descriptor);

  void serve();

  void close();

  std::string hostPort() { return messenger_.hostPort(); }

 private:
  friend class ProcessMaster;

  void onRequestInstance(MESSAGE(REQUEST_INSTANCE)&& message);
  void onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message);

  template <typename T>
  void send(const T& message) { messenger_.send(message.type, message); }

  worker_id id_;
  ProcessMaster& master_;
  Messenger messenger_;
};

class ProcessMaster {
 public:
  ProcessMaster(const JobConfig& config);

  void serve();

 private:
  friend class ProcessServerHandle;

  void onRequestInstance(MESSAGE(REQUEST_INSTANCE)&& message);
  void onInstanceExited(MESSAGE(INSTANCE_EXITED)&& message);

  void startInstance(instance_id parent_id, InstanceDescriptor&& descriptor);

  std::string job_name_;
  std::string job_description_;

  std::string bytecode_;
  MetaData metadata_;

  ProcessTree process_tree_;

  worker_id next_worker_to_use_ = 0;
  std::vector<std::unique_ptr<ProcessServerHandle>> workers_;
};
