#pragma once

#include "../network.h"
#include "config.h"

#include <memory>
#include <stdint.h>
#include <util/messenger.h>
#include <util/socket.h>
#include <vector>

class ProcessServerHandle {
 public:
  ProcessServerHandle(Socket&& socket, int32_t data_start, std::string&& data,
                      std::string bytecode);

  void startInstance(int32_t workspace_pointer, int32_t instruction_pointer,
                     int32_t space_needed, int32_t instance_id);
 private:
  template <typename T>
  void send(const T& message) { messenger_.send(message.type, message); }

  Messenger messenger_;
};

class ProcessMaster {
 public:
  ProcessMaster(const JobConfig& config);

  void serve();
 private:
  int32_t data_end_;

  std::vector<std::unique_ptr<ProcessServerHandle>> workers_;
};
