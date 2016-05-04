#pragma once

#include <stdint.h>

#include <map>
#include <string>
#include <vector>

namespace JSON {
  enum class Type {
    NULL_VALUE,
    BOOL,
    INT,
    FLOAT,
    STRING,
    OBJECT,
    ARRAY
  };

  class Node;
  typedef std::map<std::string, Node> Object;
  typedef std::vector<Node> Array;

  class Node {
   public:
    Node();
    Node(bool bool_value);
    Node(int64_t int_value);
    Node(double float_value);
    Node(const std::string &string_value);
    Node(const Array &array_value);
    Node(const Object &object_value);
    Node(const Node &node);
    Node(Node &&node);
    ~Node();

    bool asBool() const;
    int32_t asInt32() const;
    int64_t asInt64() const;
    float asFloat() const;
    double asDouble() const;
    std::string& asString() const;
    Array& asArray() const;
    Object& asObject() const;

    const Node &operator=(bool bool_value);
    const Node &operator=(int64_t int_value);
    const Node &operator=(double float_value);
    const Node &operator=(const std::string &string_value);
    const Node &operator=(const Array &array_value);
    const Node &operator=(const Object &object_value);
    const Node &operator=(const Node& node);

    Type type() const;

   private:
    void cleanup();

    void checkType(Type expected) const;

    Type type_;
    union {
      bool bool_value_;
      int64_t int_value_;
      double float_value_;
      std::string *string_value_;
      Object *object_value_;
      Array *array_value_;
    };
  };

  std::string typeString(Type type);

  void parse(std::string text, Node *output);
  Node parse(std::string text);

  std::string stringify(const Node &object, bool pretty = false);
}
