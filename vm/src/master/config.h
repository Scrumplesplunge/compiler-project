#pragma once

#include <string>
#include <vector>

struct WorkerAddress {
  std::string host;
  int port;
};

struct JobConfig {
  std::string name;
  std::string description;
  std::string bytecode_file;
  std::string metadata_file;
  std::vector<WorkerAddress> workers;
};

JobConfig loadConfig(std::string job_file);
