#pragma once

#include <string>

struct MetaData {
  std::string static_data;
  int32_t root_process_size;
  std::string assembly_file;
};

MetaData loadMetaData(std::string metadata_file);
