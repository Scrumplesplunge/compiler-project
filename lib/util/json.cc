#include "json.h"

#include <ctype.h>
#include <math.h>

#include <iostream>
#include <sstream>
#include <stdexcept>

using std::invalid_argument;
using std::istringstream;
using std::logic_error;
using std::string;
using std::to_string;

namespace JSON {
  Node::Node() {
    type_ = Type::NULL_VALUE;
  }

  Node::Node(bool bool_value) {
    type_ = Type::BOOL;
    bool_value_ = bool_value;
  }

  Node::Node(int64_t int_value) {
    type_ = Type::INT;
    int_value_ = int_value;
  }

  Node::Node(double float_value) {
    type_ = Type::FLOAT;
    float_value_ = float_value;
  }

  Node::Node(const string &string_value) {
    type_ = Type::STRING;
    string_value_ = new string(string_value);
  }

  Node::Node(const Array &array_value) {
    type_ = Type::ARRAY;
    array_value_ = new Array(array_value);
  }

  Node::Node(const Object &object_value) {
    type_ = Type::OBJECT;
    object_value_ = new Object(object_value);
  }

  Node::Node(const Node &node) {
    type_ = node.type_;
    switch (type_) {
      case Type::NULL_VALUE:
        break;
      case Type::BOOL:
        bool_value_ = node.bool_value_;
        break;
      case Type::INT:
        int_value_ = node.int_value_;
        break;
      case Type::FLOAT:
        float_value_ = node.float_value_;
        break;
      case Type::STRING:
        string_value_ = new string(*node.string_value_);
        break;
      case Type::ARRAY:
        array_value_ = new Array(*node.array_value_);
        break;
      case Type::OBJECT:
        object_value_ = new Object(*node.object_value_);
        break;
    }
  }

  Node::Node(Node &&node) {
    type_ = node.type_;
    node.type_ = Type::NULL_VALUE;
    switch (type_) {
      case Type::NULL_VALUE:
        break;
      case Type::BOOL:
        bool_value_ = node.bool_value_;
        break;
      case Type::INT:
        int_value_ = node.int_value_;
        break;
      case Type::FLOAT:
        float_value_ = node.float_value_;
        break;
      case Type::STRING:
        string_value_ = node.string_value_;
        break;
      case Type::ARRAY:
        array_value_ = node.array_value_;
        break;
      case Type::OBJECT:
        object_value_ = node.object_value_;
        break;
    }
  }

  Node::~Node() {
    cleanup();
  }

  bool Node::asBool() const {
    checkType(Type::BOOL);
    return bool_value_;
  }

  int32_t Node::asInt32() const {
    checkType(Type::INT);
    return static_cast<int32_t>(int_value_);
  }

  int64_t Node::asInt64() const {
    checkType(Type::INT);
    return int_value_;
  }

  float Node::asFloat() const {
    if (type_ == Type::INT)
      return static_cast<float>(int_value_);
    checkType(Type::FLOAT);
    return static_cast<float>(float_value_);
  }

  double Node::asDouble() const {
    if (type_ == Type::INT)
      return static_cast<double>(int_value_);
    checkType(Type::FLOAT);
    return float_value_;
  }

  std::string& Node::asString() const {
    checkType(Type::STRING);
    return *string_value_;
  }

  Array& Node::asArray() const {
    checkType(Type::ARRAY);
    return *array_value_;
  }

  Object& Node::asObject() const {
    checkType(Type::OBJECT);
    return *object_value_;
  }

  const Node &Node::operator=(bool bool_value) {
    type_ = Type::BOOL;
    bool_value_ = bool_value;
    return *this;
  }

  const Node &Node::operator=(int64_t int_value) {
    type_ = Type::INT;
    int_value_ = int_value;
    return *this;
  }

  const Node &Node::operator=(double float_value) {
    type_ = Type::FLOAT;
    float_value_ = float_value;
    return *this;
  }

  const Node &Node::operator=(const string &string_value) {
    cleanup();
    type_ = Type::STRING;
    string_value_ = new string(string_value);
    return *this;
  }

  const Node &Node::operator=(const Array &array_value) {
    cleanup();
    type_ = Type::ARRAY;
    array_value_ = new Array(array_value);
    return *this;
  }

  const Node &Node::operator=(const Object &object_value) {
    cleanup();
    type_ = Type::OBJECT;
    object_value_ = new Object(object_value);
    return *this;
  }

  const Node &Node::operator=(const Node &node) {
    cleanup();
    type_ = node.type_;
    switch (type_) {
      case Type::NULL_VALUE:
        break;
      case Type::BOOL:
        bool_value_ = node.bool_value_;
        break;
      case Type::INT:
        int_value_ = node.int_value_;
        break;
      case Type::FLOAT:
        float_value_ = node.float_value_;
        break;
      case Type::STRING:
        string_value_ = new string(*node.string_value_);
        break;
      case Type::ARRAY:
        array_value_ = new Array(*node.array_value_);
        break;
      case Type::OBJECT:
        object_value_ = new Object(*node.object_value_);
        break;
    }
    return *this;
  }

  Type Node::type() const {
    return type_;
  }

  void Node::cleanup() {
    switch (type_) {
      case Type::STRING:
        delete string_value_;
        break;
      case Type::ARRAY:
        delete array_value_;
        break;
      case Type::OBJECT:
        delete object_value_;
        break;
      default:
        break;
    } 
  }

  void Node::checkType(Type expected) const {
    if (type_ != expected) {
      throw logic_error(
          "JSON Error: Tried to use " + typeString(type_) + " as " + 
          typeString(expected) + ".");
    }
  }

  string typeString(Type type) {
    switch (type) {
      default:
      case Type::NULL_VALUE:
        return "null";
      case Type::BOOL:
        return "bool";
      case Type::INT:
        return "integer";
      case Type::FLOAT:
        return "floating point";
      case Type::STRING:
        return "string";
      case Type::ARRAY:
        return "array";
      case Type::OBJECT:
        return "object";
    }
  }

  static void matchChar(
      const char *input, int64_t *pos, int64_t length, char c) {
    if (input[*pos] != c) {
      throw invalid_argument(
          "Expected '" + string({c}) + "' at character " + to_string(*pos) +
          ".");
    }
    (*pos)++;
  }

  static void checkNotEnd(
      const char *input, int64_t *pos, int64_t length) {
    if (*pos == length)
      throw invalid_argument("Unexpected end of input.");
  }

  static void skipWhitespace(
      const char *input, int64_t *pos, int64_t length) {
    while (*pos < length && isspace(input[*pos])) (*pos)++;
    checkNotEnd(input, pos, length);
  }

  static int matchHex(
      const char *input, int64_t *pos, int64_t length) {
    char c = input[*pos];
    if ('0' <= c && c <= '9') {
      return c - '0';
    } else if ('a' <= c && c <= 'f') {
      return c - 'a' + 10;
    } else if ('A' <= c && c <= 'F') {
      return c - 'A' + 10;
    } else {
      throw invalid_argument(
          "Expected hexadecimal digit at character " + to_string(*pos) + ".");
    }
  }

  static string encodeAsUTF8(int value) {
    string out;
    if (value <= 0x7F) {
      out += static_cast<char>(value);
    } else if (value <= 0x7FF) {
      out += static_cast<char>(0xC0 | (value >> 6));
      out += static_cast<char>(0x80 | (value & 0x3F));
    } else if (value <= 0xFFFF) {
      out += static_cast<char>(0xD0 | (value >> 12));
      out += static_cast<char>(0x80 | ((value >> 6) & 0x3F));
      out += static_cast<char>(0x80 | (value & 0x3F));
    } else {
      throw invalid_argument(
          "Only codepoints in the Basic Multilingual Plane can be represented "
          "as hexadecimal sequences.");
    }
    return out;
  }

  static void parseString(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    skipWhitespace(input, pos, length);
    matchChar(input, pos, length, '"');
    string value;

    while (*pos < length && input[*pos] != '"') {
      if (input[*pos] == '\\') {
        (*pos)++;
        checkNotEnd(input, pos, length);
        char c = input[(*pos)++];
        switch (c) {
          case '"':
          case '\\':
          case '/':
            value.push_back(c);
            break;
          case 'b':
            value.push_back('\b');
            break;
          case 'f':
            value.push_back('\f');
            break;
          case 'n':
            value.push_back('\n');
            break;
          case 'r':
            value.push_back('\r');
            break;
          case 't':
            value.push_back('\t');
            break;
          case 'u': {
            int code = 0;
            for (int i = 0; i < 4; i++) {
              code *= 16;
              checkNotEnd(input, pos, length);
              code += matchHex(input, pos, length);
            }
            try {
              value += encodeAsUTF8(code);
            } catch (...) {
              throw invalid_argument("Invalid unicode sequence at character " +
                                     to_string(*pos) + ".");
            }
            break;
          }
        }
      } else {
        value.push_back(input[*pos]);
        (*pos)++;
      }
    }
    (*pos)++;
    *output = value;
  }

  static void parseNode(
      const char *input, int64_t *pos, int64_t length, Node *output);
    
  static void parseObject(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    skipWhitespace(input, pos, length);
    matchChar(input, pos, length, '{');
    *output = Object();
    skipWhitespace(input, pos, length);

    // Loop over all members of the object.
    while (input[*pos] != '}') {
      Node index;

      // Match the index.
      parseString(input, pos, length, &index);
      skipWhitespace(input, pos, length);
      matchChar(input, pos, length, ':');
      skipWhitespace(input, pos, length);

      // Match the value.
      parseNode(input, pos, length, &output->asObject()[index.asString()]);

      // Expect either another element, or the end.
      skipWhitespace(input, pos, length);
      if (input[*pos] == '}') break;
      matchChar(input, pos, length, ',');
      skipWhitespace(input, pos, length);
    }
    (*pos)++;
  }

  static void parseArray(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    skipWhitespace(input, pos, length);
    matchChar(input, pos, length, '[');
    *output = Array();
    skipWhitespace(input, pos, length);

    // Loop over all the contents.
    while (input[*pos] != ']') {
      // Match the value.
      output->asArray().push_back(JSON::Node());
      parseNode(input, pos, length, &output->asArray().back());

      // Expect another element, or the end.
      skipWhitespace(input, pos, length);
      if (input[*pos] == ']') break;
      matchChar(input, pos, length, ',');
      skipWhitespace(input, pos, length);
    }
    (*pos)++;
  }

  static void matchNumber(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    skipWhitespace(input, pos, length);

    bool negative = false;
    string int_part;
    string frac_part;

    bool exp_negative = false;
    string exp_part;

    bool is_float = false;

    int number_pos = *pos;
    
    // Read the sign of the number.
    if (input[*pos] == '-') {
      negative = true;
      (*pos)++;
    }

    // Read the integer part.
    while ('0' <= input[*pos] && input[*pos] <= '9')
      int_part += input[(*pos)++];
    if (int_part.length() == 0) {
      throw invalid_argument(
          "Invalid number at character " + to_string(number_pos) + ".");
    }
    if (int_part.length() > 1 && int_part[0] == '0') {
      throw invalid_argument(
          "Number should not have leading '0' at character " +
          to_string(number_pos) + ".");
    }

    if (int_part.length() > 19) {
      // No integer with more than 19 digits can be stored in an int64_t.
      is_float = true;
    } else {
      // No integer with 19 digits that is greater than 2^63 (or 2^63-1 if
      // positive)
      uint64_t int_value;
      istringstream reader(int_part);
      reader >> int_value;
      if (int_value > (1ULL << 63) - (negative ? 0 : 1))
        is_float = true;
    }

    if (input[*pos] == '.') {
      is_float = true;
      (*pos)++;

      // Read the fractional part.
      while ('0' <= input[*pos] && input[*pos] <= '9')
        frac_part += input[(*pos)++];
      if (frac_part.length() == 0) {
        throw invalid_argument(
            "Invalid number at character " + to_string(number_pos) + ".");
      }
    }

    if (input[*pos] == 'e' || input[*pos] == 'E') {
      is_float = true;
      (*pos)++;

      // Read the sign of the exponent.
      if (input[*pos] == '+') {
        (*pos)++;
      } else if (input[*pos] == '-') {
        exp_negative = true;
        (*pos)++;
      }

      // Read the value of the exponent.
      while ('0' <= input[*pos] && input[*pos] <= '9')
        exp_part += input[(*pos)++];
    }

    if (is_float) {
      // Interpret the significand.
      istringstream significand(int_part + "." + frac_part);
      double value;
      significand >> value;
      if (negative) value = -value;

      // Interpret the exponent.
      istringstream exponent(exp_part);
      double e;
      exponent >> e;
      if (exp_negative) e = -e;

      // Put it together.
      value *= pow(10.0, e);

      *output = value;
    } else {
      // Interpret the integer.
      istringstream number(int_part);
      uint64_t value;
      number >> value;
      if (negative) value = -value;

      *output = static_cast<int64_t>(value);
    }
  }

  static bool tryMatchNumber(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    try {
      matchNumber(input, pos, length, output);
      return true;
    } catch (...) {
      return false;
    }
  }

  static void parseNode(
      const char *input, int64_t *pos, int64_t length, Node *output) {
    skipWhitespace(input, pos, length);

    switch (input[*pos]) {
      case '{':
        parseObject(input, pos, length, output);
        break;
      case '[':
        parseArray(input, pos, length, output);
        break;
      case '"':
        parseString(input, pos, length, output);
        break;
      case 't':
      case 'f':
      case 'n':
        if (string(input, *pos, 4) == "true") {
          *output = true;
          *pos += 4;
          break;
        } else if (string(input, *pos, 5) == "false") {
          *output = false;
          *pos += 5;
          break;
        } else if (string(input, *pos, 4) == "null") {
          *output = Node();
          *pos += 4;
          break;
        }
        // Otherwise, overflow into default case.
      default:
        // TODO: Handle numbers here, and reach the exception otherwise.
        if (!tryMatchNumber(input, pos, length, output)) {
          throw invalid_argument("Invalid token at character " +
                                 to_string(*pos) + ".");
        }
        break;
    }
  }

  void parse(string text, Node *output) {
    const char *input = text.c_str();
    int64_t pos = 0;
    int64_t length = text.length();
    parseNode(input, &pos, length, output);
  }

  Node parse(string text) {
    Node output;
    parse(text, &output);
    return output;
  }

  string escapeString(string input) {
    string output = "\"";
    output.reserve(input.size());
    for (char c : input) {
      switch (c) {
        case '"':
          output += "\\\"";
          break;
        case '\\':
          output += "\\\\";
          break;
        case '\b':
          output += "\\b";
          break;
        case '\f':
          output += "\\f";
          break;
        case '\n':
          output += "\\n";
          break;
        case '\r':
          output += "\\r";
          break;
        case '\t':
          output += "\\t";
          break;
        default:
          output += c;
      }
    }
    output += "\"";
    return output;
  }

  static string stringifyPretty(const Node &object, int indent_amount) {
    string indent(indent_amount, ' ');
    switch (object.type()) {
      case Type::NULL_VALUE:
        return "null";
      case Type::BOOL:
        return object.asBool() ? "true" : "false";
      case Type::INT:
        return to_string(object.asInt64());
      case Type::FLOAT:
        return to_string(object.asDouble());
      case Type::STRING:
        return escapeString(object.asString());
      case Type::ARRAY: {
        const Array &array = object.asArray();
        if (array.size() == 0) return "[]";
        string output = "[\n";
        for (const auto &item : array) {
          output += indent + "  " + stringifyPretty(item, indent_amount + 2) +
                    ",\n";
        }
        output.pop_back();
        output.pop_back();
        output += "\n" + indent + "]";
        return output;
      }
      case Type::OBJECT: {
        const Object &obj = object.asObject();
        if (obj.size() == 0) return "{}";
        string output = "{\n";
        for (const auto &item : obj) {
          output += indent + "  " + escapeString(item.first) + " : " +
                    stringifyPretty(item.second, indent_amount + 2) + ",\n";
        }
        output.pop_back();
        output.pop_back();
        output += "\n" + indent + "}";
        return output;
      }
      default:
        throw logic_error("This should never happen.");
    }
  }

  string stringify(const Node &object, bool pretty) {
    if (pretty) return stringifyPretty(object, 0);
    switch (object.type()) {
      case Type::NULL_VALUE:
        return "null";
      case Type::BOOL:
        return object.asBool() ? "true" : "false";
      case Type::INT:
        return to_string(object.asInt64());
      case Type::FLOAT:
        return to_string(object.asFloat());
      case Type::STRING:
        return escapeString(object.asString());
      case Type::ARRAY: {
        string output = "[";
        bool first = true;
        for (const auto &item : object.asArray()) {
          if (!first) output += ",";
          first = false;
          output += stringify(item);
        }
        output += "]";
        return output;
      }
      case Type::OBJECT: {
        string output = "{";
        bool first = true;
        for (const auto &item : object.asObject()) {
          if (!first) output += ",";
          first = false;
          output += escapeString(item.first) + ":" + stringify(item.second);
        }
        output += "}";
        return output;
      }
      default:
        throw logic_error("This should never happen.");
    }
  }
}
