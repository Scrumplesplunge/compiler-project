#pragma once

#include "binary.h"
#include "socket.h"

#include <functional>
#include <memory>
#include <mutex>
#include <stdint.h>
#include <unordered_map>
#include <vector>

// All message type classes should be representable as this type.
typedef uint64_t MessageType;

// All messages should be sub-classes of this.
struct MessageBase {
  MessageBase(MessageType type)
      : message_type(type) {}
  virtual ~MessageBase() = default;

  virtual void encode(BinaryWriter& writer) const = 0;
  virtual void decode(BinaryReader& reader) = 0;

  const MessageType message_type;
};

typedef std::function<void(MessageType, BinaryReader&)> MessageHandlerBase;

void no_handler(MessageType type, BinaryReader& message);

// Base class for all messengers.
class MessengerBase {
 public:
  MessengerBase(Socket&& socket,
                MessageHandlerBase default_handler = no_handler)
      : socket_(std::move(socket)), default_handler_(default_handler) {}
  virtual ~MessengerBase() = default;

  void on(MessageType type, MessageHandlerBase handler);
  void send(const MessageBase& message);

  // Wait for a message to be received and handle it.
  void poll();

  // Repeatedly handle messages until the socket closes.
  void serve();

 private:
  void unlocked_poll();

  std::mutex send_lock_, receive_lock_;
  Socket socket_;
  MessageHandlerBase default_handler_;
  std::unordered_map<MessageType, MessageHandlerBase> handlers_;
};

// Messages from type class T.
template <typename T>
struct Message : public MessageBase {
  Message(T type)
      : MessageBase(static_cast<MessageType>(type)) {}
  virtual ~Message() = default;

  void encode(BinaryWriter& writer) const override { }
  void decode(BinaryReader& reader) override { }

  T type() const { return static_cast<T>(message_type); }
};

// Basic payload message from type class T, with payload type P.
template <typename T, typename P>
struct PayloadMessage : public Message<T> {
  PayloadMessage(T type, P&& value)
      : Message<T>(type), payload(value) {}

  void encode(BinaryWriter& writer) const override { writer.write<P>(payload); }
  void decode(BinaryReader& reader) override { payload = reader.read<P>(); }

  P payload;
};

// Message handlers for message of type M.
template <typename T, typename M>
using MessageHandler = std::function<void(T, const M&)>;

// Messenger for messages of type class T.
template <typename T>
class Messenger : public MessengerBase {
 public:
  Messenger(Socket&& socket)
      : MessengerBase(std::move(socket)) {}
  virtual ~Messenger() = default;

  template <typename M>
  void on(T type, MessageHandler<T, M> handler) {
    // Wrap the handler with an appropriate decoder.
    MessageHandlerBase raw_handler =
        [handler](MessageType type, BinaryReader& message_reader) {
      M message;
      message.decode(message_reader);
      return handler(static_cast<T>(type), message);
    };
    MessengerBase::on(static_cast<MessageType>(type), raw_handler);
  }
};
