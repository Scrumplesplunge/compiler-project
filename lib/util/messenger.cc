#include "messenger.h"

#include <sstream>

using namespace std;

void no_handler(MessageType type, BinaryReader& message) {
  cerr << "Warning: Message of type " << type << " unhandled.\n";
}

void MessengerBase::on(MessageType type, MessageHandlerBase handler) {
  // Add the handler to the list for this message type.
  if (handlers_.count(type) > 0) {
    throw logic_error("Registered multiple handlers for message type " +
                      to_string(type));
  }
  handlers_.emplace(type, handler);
}

void MessengerBase::send(const MessageBase& message) {
  // Serialise the message.
  string bytes;
  {
    ostringstream builder;
    StandardOutputStream stream(builder);
    BinaryWriter writer(stream);
    message.encode(writer);
    bytes = builder.str();
  }

  // Send the message header, followed by the message.
  unique_lock<mutex> lock(send_lock_);
  BinaryWriter writer(socket_);
  writer.writeVarUint(message.message_type);
  writer.writeString(bytes);
}

void MessengerBase::poll() {
  unique_lock<mutex> lock(receive_lock_);
  unlocked_poll();
}

void MessengerBase::serve() {
  unique_lock<mutex> lock(receive_lock_);
  try {
    while (true) unlocked_poll();
  } catch (const runtime_error& error) { }
}

void MessengerBase::unlocked_poll() {
  // Read the message from the socket.
  BinaryReader reader(socket_);
  MessageType type = reader.readVarUint();
  istringstream bytes(reader.readString());
  StandardInputStream stream(bytes);
  BinaryReader message_reader(stream);

  if (handlers_.count(type) == 0) {
    // No handler is set for this message type.
    default_handler_(type, message_reader);
  } else {
    // Use the configured handler.
    handlers_[type](type, message_reader);
  }
}
