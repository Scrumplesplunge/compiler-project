#include "messenger.h"

#include "atomic_output.h"

#include <sstream>

using namespace std;

struct MessageHeader {
  MessageTypeID type;
  uint64_t length;
};

template <>
void BinaryReader::read(MessageHeader* header) {
  header->type = readVarUint();
  header->length = readVarUint();
}

template <>
void BinaryWriter::write(const MessageHeader& header) {
  writeVarUint(header.type);
  writeVarUint(header.length);
}


Messenger::Messenger(Socket&& socket)
    : socket_(std::move(socket)), reader_(socket_), writer_(socket_) {
  socket_.set_nagle(false);
}

Messenger::Messenger(Messenger&& other)
    : socket_(std::move(other.socket_)), reader_(socket_), writer_(socket_) {}

void Messenger::poll() {
  unique_lock<mutex> reader_lock(reader_mutex_);
  unlockedPoll(reader_lock);
}

void Messenger::serve() {
  unique_lock<mutex> reader_lock(reader_mutex_);
  while (true) unlockedPoll(reader_lock);
}

void Messenger::close() {
  socket_.closeInput();
  socket_.closeOutput();
}

void Messenger::sendBytes(MessageTypeID type, const string& bytes) {
  // Construct the message.
  string message;
  {
    ostringstream builder;
    StandardOutputStream output(builder);
    BinaryWriter writer(output);
    writer_.write(MessageHeader{type, bytes.length()});
    writer_.writeBytes(bytes.c_str(), bytes.length());
    message = builder.str();
  }

  // Send it.
  unique_lock<mutex> writer_lock(writer_mutex_);
  writer_.writeBytes(message.c_str(), message.length());
}

void Messenger::on(MessageTypeID type, MessageHandlerBase handler) {
  if (!handlers_.emplace(type, handler).second) {
    // Overriding an existing handler.
    throw logic_error(
        "Overriding existing handler for type ID " + to_string(type) + ".");
  }
}

void Messenger::unlockedPoll(unique_lock<mutex>& reader_lock) {
  BinaryReader reader(socket_);

  verr << "Waiting for message..\n";
  MessageHeader header;
  reader_.read(&header);
  verr << "INCOMING: MESSAGE<" << header.type << ">\n";
  if (handlers_.count(header.type) == 0) {
    cerr << "Warning: Not handling message with type " << header.type << ".\n";
    // Unrecognised ID. Discard the message.
    const uint64_t BUFFER_SIZE = 32;
    char buffer[32];
    uint64_t length = header.length;
    
    while (length > 0) {
      if (length > BUFFER_SIZE) {
        reader.readBytes(buffer, BUFFER_SIZE);
        length -= BUFFER_SIZE;
      } else {
        reader.readBytes(buffer, length);
        length = 0;
      }
    }
  }

  // Handle the message.
  handlers_.at(header.type)();
}
