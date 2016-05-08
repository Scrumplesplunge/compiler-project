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
  ProcessServerHandle(Socket&& socket, std::string data, std::string bytecode);

 private:
  Messenger messenger_;
};

class ProcessMaster {
 public:
  ProcessMaster(const JobConfig& config);

  void serve();
 private:
  std::vector<std::unique_ptr<ProcessServerHandle>> workers_;
};
