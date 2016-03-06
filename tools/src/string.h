#pragma once

#include <ctype.h>
#include <string>

std::string toUpper(std::string input) {
  std::string out;
  for (char c : input) out.push_back(toupper(c));
  return out;
}

std::string toLower(std::string input) {
  std::string out;
  for (char c : input) out.push_back(tolower(c));
  return out;
}
