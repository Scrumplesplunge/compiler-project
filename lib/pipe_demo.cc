#include "util/pipe.h"
#include "util/stream.h"

#include <algorithm>
#include <chrono>
#include <string>
#include <thread>

using namespace std;
using namespace std::chrono_literals;

string data =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed gravida\n"
    "lacus lectus, id eleifend quam tristique in. Integer consectetur congue\n"
    "efficitur.  Maecenas lacinia tincidunt ipsum, ut gravida dolor tempor\n"
    "tincidunt. Aenean eu pulvinar sem, quis interdum mi. Nullam condimentum\n"
    "venenatis nunc, at mattis nisl consectetur vitae. Donec porta mauris non\n"
    "lorem gravida, sit amet aliquet enim tincidunt. Duis pharetra vitae enim\n"
    "vitae pharetra. Etiam convallis orci nec ante volutpat congue. Nullam\n"
    "finibus nibh vitae elementum lobortis.\n"
    "\n"
    "Praesent laoreet ac arcu sit amet bibendum. Vivamus ornare vulputate\n"
    "lacus quis ultrices. Vivamus volutpat ut sapien id consectetur. Fusce\n"
    "vitae rhoncus tortor.  Curabitur a nisl tortor. Vivamus placerat tempor\n"
    "enim eu volutpat. Cras non aliquam arcu. Cras ut lectus vitae libero\n"
    "luctus ultricies. Proin porttitor felis et urna sagittis, id suscipit\n"
    "elit porttitor.\n";
const int data_length = data.length();

void producer(OutputStream& output) {
  // Write increasingly long blobs.
  int i = 0;
  int j = 1;
  while (i < data_length) {
    int i2 = min(i + j, data_length);
    output.write(data.c_str() + i, i2 - i);
    i = i2;
    j++;
    this_thread::sleep_for(100ms);
  }
  output.closeOutput();
}

void consumer(InputStream& input) {
  const int CHUNK_SIZE = 10;
  int index = 0;
  char buffer[CHUNK_SIZE];

  // Read in blocks of CHUNK_SIZE bytes.
  int length;
  while ((length = input.read(buffer, CHUNK_SIZE))) {
    cout.write(buffer, length);
    cout.flush();
    for (int i = 0; i < length; i++) {
      if (data[index + i] != buffer[i])
        throw runtime_error("Data mismatch :(");
    }
    index += length;
  }
}

int main() {
  Pipe pipe;

  thread sender(producer, ref(pipe));
  thread receiver(consumer, ref(pipe));

  sender.join();
  receiver.join();
}
