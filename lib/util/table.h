#pragma once

#include "text.h"

#include <map>
#include <string>
#include <vector>

class Table {
 public:
  Table(int columns);
  void setColumnWidth(int column, int width);
  void setCell(int row, int column, std::string value);
  void output(std::ostream& stream);

  const int DEFAULT_COLUMN_WIDTH = 40;
 private:
  typedef std::map<int, std::string> Row;
  int num_columns_;
  std::vector<int> column_widths_;
  std::map<int, Row> contents_;
};
