#include "messenger.h"

#include <sstream>

using namespace std;

void Messenger::poll() {
  unique_lock<mutex> reader_lock(reader_mutex_);
  unlockedPoll(reader_lock);
}

void Messenger::serve() {
  unique_lock<mutex> reader_lock(reader_mutex_);
  while (true) unlockedPoll(reader_lock);
}

void Messenger::writeHeader(
    const MessageHeader& header, unique_lock<mutex>& writer_lock) {
  writer_.writeVarUint(header.type);
  writer_.writeVarUint(header.length);
}

MessageHeader Messenger::readHeader(unique_lock<mutex>& reader_lock) {
  MessageHeader out;
  out.type = reader_.readVarUint();
  out.length = reader_.readVarUint();
  return out;
}

void Messenger::sendBytes(MessageTypeID type, const string& bytes) {
  MessageHeader header{type, bytes.length()};
  unique_lock<mutex> writer_lock(writer_mutex_);
  writeHeader(header, writer_lock);
  writer_.writeBytes(bytes.c_str(), bytes.length());
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

  MessageHeader header = readHeader(reader_lock);
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
