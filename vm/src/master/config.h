#pragma once

#include <string>
#include <vector>

struct WorkerAddress {
  std::string host;
  int port;
};

struct JobConfig {
  std::string bytecode_file;
  std::string data_file;
  std::vector<WorkerAddress> workers;
};

JobConfig loadConfig(std::string job_file);
