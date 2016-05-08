#include "text.h"

using namespace std;

wrap_iterator wrap_generator::begin() {
  return wrap_iterator(input_, wrap_width_);
}

wrap_iterator wrap_generator::end() {
  return wrap_iterator(input_, wrap_width_, input_.length());
}

const string& wrap_iterator::operator*() const {
  return this_line_;
}

const string* wrap_iterator::operator->() const {
  return &this_line_;
}

void wrap_iterator::operator++() {
  int length = input_.length();
  int start = end_;
  // Skip all leading whitespace.
  while (start < length && input_[start] == ' ') start++;
  int end = start + wrap_width_;
  if (end > length) end = length;
  // Scan for newlines and spaces.
  int split = end;
  for (int i = start; i < end; i++) {
    if (input_[i] == '\n') {
      // The line finishes itself.
      position_ = start;
      end_ = i + 1;
      this_line_ = input_.substr(start, i - start);
      return;
    }
    if (input_[i] == ' ') split = i;
  }
  // Check if line fits without breaking.
  if (end == length) split = end;
  // If the loop terminated, there was no space for the whole width.
  position_ = start;
  end_ = split;
  this_line_ = input_.substr(start, split - start);
}

bool wrap_iterator::operator==(const wrap_iterator& that) {
  return position_ == that.position_;
}

bool wrap_iterator::operator!=(const wrap_iterator& that) {
  return position_ != that.position_;
}

void pad(ostream& stream, int width) {
  for (int i = 0; i < width; i++) stream << " ";
}

wrap_generator wrap(const string& input, int width) {
  return wrap_generator(input, width);
}

void arrange(ostream& stream, string input, int indent, int limit) {
  arrange(stream, input, indent, indent, limit);
}

void arrange(ostream& stream, string input,
             int first_line_indent, int indent_amount, int limit) {
  // Wrap all lines, and then output them with indentation.
  for (const string& line : wrap(input, limit - indent_amount)) {
    pad(stream, indent_amount);
    indent_amount = first_line_indent;
    stream << line << '\n';
  }
}
