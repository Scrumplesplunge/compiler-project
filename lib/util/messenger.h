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

struct MessageBase {
  MessageBase(MessageType type)
      : message_type(type) {}
  virtual ~MessageBase() = default;

  virtual void encode(BinaryWriter& writer) const = 0;
  virtual void decode(BinaryReader& reader) = 0;

  const MessageType message_type;
};

// Basic payload message with payload type P.
template <typename P>
struct PayloadMessageBase : public MessageBase {
  PayloadMessageBase(MessageType type, P value)
      : MessageBase(type), payload(value) {}
  virtual ~PayloadMessageBase() = default;

  void encode(BinaryWriter& writer) const override { writer.write<P>(payload); }
  void decode(BinaryReader& reader) override { payload = reader.read<P>(); }

  P payload;
};

// Message of the specified type.
template <MessageType type>
struct Message : public MessageBase {
  Message()
      : MessageBase(type) {}
  virtual ~Message() = default;
};

// Payload message of the specified type.
template <MessageType type, typename P>
struct PayloadMessage : public PayloadMessageBase<P> {
  PayloadMessage()
      : PayloadMessage(P()) {}
  PayloadMessage(P value)
      : PayloadMessageBase<P>(type, value) {}
  virtual ~PayloadMessage() = default;
};

typedef std::function<void(MessageType, BinaryReader&)> MessageHandlerBase;

void no_handler(MessageType type, BinaryReader& message);

// Base class for all messengers.
class MessengerBase {
 public:
  MessengerBase(Socket socket,
                MessageHandlerBase default_handler = no_handler)
      : socket_(std::move(socket)), default_handler_(default_handler) {}
  MessengerBase(MessengerBase&& other)
      : socket_(std::move(other.socket_)),
        default_handler_(other.default_handler_) {}
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

// Message handlers for message of type M.
template <typename M>
using MessageHandler = std::function<void(const M&)>;

class Messenger : public MessengerBase {
 public:
  Messenger(Socket socket)
      : MessengerBase(std::move(socket)) {}
  Messenger(Messenger&& other)
      : MessengerBase(std::move(reinterpret_cast<MessengerBase&>(other))) {}
  virtual ~Messenger() = default;

  template <typename M>
  void on(MessageType type, MessageHandler<M> handler) {
    // Wrap the handler with an appropriate decoder.
    MessageHandlerBase raw_handler =
        [handler](MessageType type, BinaryReader& message_reader) {
      M message;
      message.decode(message_reader);
      return handler(message);
    };
    MessengerBase::on(type, raw_handler);
  }
};
