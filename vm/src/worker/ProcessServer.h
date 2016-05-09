#pragma once

#include "../network.h"

#include <memory>
#include <stdint.h>
#include <string>
#include <util/messenger.h>
#include <util/socket.h>

class ProcessServer {
 public:
  ProcessServer(Socket&& socket);

  void serve();

 private:
  void onStartProcessServer(MESSAGE(START_PROCESS_SERVER)&& message);
  void onStartInstance(MESSAGE(START_INSTANCE)&& message);

  bool is_ready_;

  int32_t data_start_, data_end_;
  std::unique_ptr<int32_t[]> data_;

  std::string bytecode_;

  Messenger messenger_;
};
