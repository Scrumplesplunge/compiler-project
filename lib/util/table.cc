#include "table.h"

#include <sstream>

using namespace std;

Table::Table(int columns)
    : num_columns_(columns), column_widths_(columns, DEFAULT_COLUMN_WIDTH) {}

void Table::setColumnWidth(int column, int width) {
  column_widths_[column] = width;
}

void Table::setCell(int row, int column, string value) {
  contents_[row][column] = value;
}

void Table::output(std::ostream& stream) {
  for (const auto& row : contents_) {
    // Fetch the wrap generators for every column.
    vector<wrap_iterator> iterators;
    vector<wrap_iterator> ends;
    string empty_string;
    for (int i = 0; i < num_columns_; i++) {
      auto cell = row.second.find(i);
      // Wrap either the value, or empty space.
      wrap_generator generator =
          wrap(cell != row.second.end() ? cell->second : empty_string,
               column_widths_[i]);
      iterators.push_back(generator.begin());
      ends.push_back(generator.end());
    }
    // Output lines until all cells are done.
    int cells_output;
    while (true) {
      cells_output = 0;
      stringstream row_output;
      for (int i = 0; i < num_columns_; i++) {
        if (iterators[i] != ends[i]) {
          cells_output++;
          row_output << *iterators[i];
          pad(row_output, column_widths_[i] - iterators[i]->length());
          ++iterators[i];
        } else {
          pad(row_output, column_widths_[i]);
        }
      }
      if (cells_output == 0) break;
      stream << row_output.str() << "\n";
    }
  }
}
