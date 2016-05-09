#pragma once

#include <string>

struct MetaData {
  int32_t memory_start, workspace_pointer, memory_size;
  std::string assembly_file;
  std::string static_data;
};

MetaData loadMetaData(std::string metadata_file);
