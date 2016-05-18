#include "messenger.h"

#include "atomic_output.h"

#include <sstream>

using namespace std;

struct MessageHeader {
  MessageTypeID type;
};

template <>
void BinaryReader::read(MessageHeader* header) {
  header->type = readVarUint();
}

template <>
void BinaryWriter::write(const MessageHeader& header) {
  writeVarUint(header.type);
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
    ostringstream builder(std::ios::binary);
    StandardOutputStream output(builder);
    BinaryWriter writer(output);
    writer.write(MessageHeader{type});
    writer.writeString(bytes);
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

  MessageHeader header;
  reader_.read(&header);
  string bytes = reader_.readString();

  if (handlers_.count(header.type) == 0) {
    cerr << "Warning: Not handling message with type " << header.type << ".\n";
    // Unrecognised ID. Discard the message.
  } else {
    // Handle the message.
    istringstream temp(move(bytes), std::ios::binary);
    StandardInputStream stream(temp);
    BinaryReader reader(stream);
    handlers_.at(header.type)(reader);
  }
}
