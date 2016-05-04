#pragma once

#include <stdint.h>
#include <string>

// Computes a single hex digit from a value 0-15.
char hex(int8_t value);

// Returns an address in the format 0x%08x.
std::string addressString(int32_t address);
