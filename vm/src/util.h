#pragma once

#include <stdint.h>
#include <string>

// Computes a single hex digit from a value 0-15.
char hex(int8_t value);

// Returns an address in the format 0x%08x.
std::string addressString(int32_t address);

// Fetch the contents of a file, or throw an exception if this is not possible.
std::string getFileContents(std::string filename);
