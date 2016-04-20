#pragma once

#include <iostream>

namespace args {

template <typename T>
OptionInformation<T>::OptionInformation(
    const T& default_value, T* output, const char* name,
    const char* description)
    : GenericOption(name, description), default_value_(default_value),
      output_(output) {
  registerOption(this);
}

template <typename T>
bool OptionInformation<T>::read(int argc, char* args[], int* args_read) {
  // Default case: expect an option delimiter and single value.
  if (argc < 2) return false;
  *args_read = 1;
  std::stringstream stream(args[1]);
  stream >> *output_;
  return !stream.fail();
}

template <typename T>
const char* OptionInformation<T>::default_value() {
  // Lazily calculate the string representation of the default value.
  if (!has_default_value_string_) {
    std::stringstream stream;
    stream << default_value_;
    default_value_string_ = stream.str();
    has_default_value_string_ = true;
  }
  return default_value_string_.c_str();
}

template <>
class OptionInformation<bool> : public GenericOption {
 public:
  OptionInformation(bool default_value, bool* output, const char* name,
                    const char* description)
      : GenericOption(name, description), default_value_(default_value),
        output_(output) {
    registerOption(this);
  }

  bool read(int argc, char* args[], int* args_read) override {
    if (argc < 1) return false;
    *args_read = 0;
    *output_ = true;
    return true;
  }

  const char* default_value() override {
    return (default_value_ ? "true" : "false");
  }
 private:
  bool default_value_;
  bool* output_;
};


}  // namespace args
