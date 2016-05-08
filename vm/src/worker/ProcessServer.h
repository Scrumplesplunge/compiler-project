#pragma once

#include "../network.h"

#include <string>
#include <util/messenger.h>
#include <util/socket.h>

class ProcessServer {
 public:
  ProcessServer(Socket&& socket);

  void serve();

 private:
  void onStartProcessServer(MESSAGE(START_PROCESS_SERVER)&& message);

  bool is_ready_;
  std::string data_;
  std::string bytecode_;

  Messenger messenger_;
};
