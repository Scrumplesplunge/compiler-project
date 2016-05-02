#pragma once

#include <functional>
#include <sstream>
#include <string>

namespace args {

class GenericOption {
 public:
  GenericOption(const char* name, const char* description)
      : name_(name), description_(description) {}

  virtual bool read(int argc, char* args[], int* args_read) = 0;
  virtual const char* default_value() = 0;
  
  const char* name() { return name_; }
  const char* description() { return description_; }
 private:
  const char* name_;
  const char* description_;
};

template <typename T>
class OptionInformation : public GenericOption {
 public:
  OptionInformation(const T& default_value, T* output, const char* name,
                    const char* description);

  // Attempts to read the value of the option from the arguments. The
  // input values should be the available arguments *including* the
  // option delimiter. The return value will be true if the argument
  // was read successfully, and argc will be updated to the number of
  // *additional* arguments that were consumed.
  bool read(int argc, char* args[], int* args_read) override;

  const char* default_value() override;
 private:
  const T& default_value_;
  T* output_;
  bool has_default_value_string_ = false;
  std::string default_value_string_;
};

// Register the program usage description.
const char* registerDescription(const char* text);

// Register a new flag with the argument reader.
void registerOption(GenericOption* option);

// Read the given options, and update the arguments list to contain only the
// plain arguments (ie. those which do not have a special meaning to the
// reader).
//
// If the list of given arguments contains one of the following, then this
// function will *not* return, and will instead exit the program with code 0.
//
// --help     : Show a usage message for the program, including the list of
//              available options.
// --defaults : Show the default values for all special options.
void process(int* argc, char** args[]);

}  // namespace args

#define USAGE(text)                                                            \
    namespace args {                                                           \
      const char* description = registerDescription(text);                     \
    }

#define OPTION(type, name, default_value, description)                         \
    namespace options {                                                        \
      type name##_default = default_value;                                     \
      type name = name##_default;                                              \
      args::OptionInformation<type> _##name##_info(                            \
          name##_default, &name, #name, description);                          \
    }

#define DECLARE_OPTION(type, name)                                             \
    namespace options {                                                        \
      extern type name;                                                        \
    }                                                                          \

#define FLAG(name, description)                                                \
    OPTION(bool, name, false, description)

#define DECLARE_FLAG(name)                                                     \
    DECLARE_OPTION(bool, name)

#include "args.inl.h"
