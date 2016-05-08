#pragma once

#include <iostream>
#include <string>
#include <vector>

class wrap_iterator {
 public:
  wrap_iterator(const std::string& input, int width, int position = 0)
      : input_(input), wrap_width_(width), end_(position) {
    operator++();
  }
  const std::string& operator*() const;
  const std::string* operator->() const;
  void operator++();
  bool operator==(const wrap_iterator& that);
  bool operator!=(const wrap_iterator& that);
 private:
  const std::string& input_;
  std::string this_line_;
  int wrap_width_;
  int position_, end_;
};

// The return type of the wrap function: an iterator that produces all lines
// after wrapping. The string being wrapped *must* outlive this iterator.
class wrap_generator {
 public:
  wrap_generator(const std::string& input, int width)
      : input_(input), wrap_width_(width) {}

  wrap_iterator begin();
  wrap_iterator end();
 private:
  const std::string& input_;
  int wrap_width_;
};

// Output the given number of spaces to the stream.
void pad(std::ostream& stream, int width);

// Breaks the input text up into lines that do not exceed the given width.
wrap_generator wrap(const std::string& input, int width);

// Outputs the text to the stream, breaking it up so that each line starts with
// the given indent, and no line goes beyond the limit column.
void arrange(
    std::ostream& stream, std::string input, int indent_amount, int limit);

// Behaves like the above, but the indentation on the first line is not output.
// This allows for behaviour such as:
// cout << "foo: ";
// arrange(cout, "bar bar bar bar", 0, 5, 15);
// foo: bar bar
//      bar bar
void arrange(std::ostream& stream, std::string input,
             int first_line_indent, int indent_amount, int limit);
