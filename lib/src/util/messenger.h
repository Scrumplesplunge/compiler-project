#pragma once

#include "binary.h"
#include "socket.h"
#include "stream.h"

#include <functional>
#include <memory>
#include <mutex>
#include <sstream>
#include <stdint.h>
#include <string>
#include <unordered_map>
#include <vector>

// All message type classes should be representable as this type.
typedef uint64_t MessageTypeID;

// Base handler for incoming messages.
typedef std::function<void()> MessageHandlerBase;

// Message handlers for message of type M.
template <typename M>
using MessageHandler = std::function<void(M&&)>;

// Base class for all messengers.
class Messenger {
 public:
  Messenger(Socket&& socket);
  Messenger(Messenger&& other);

  virtual ~Messenger() = default;

  template <typename T>
  void on(MessageTypeID type, MessageHandler<T> handler) {
    // Wrap the handler with a decoder for the message.
    on(type, [this, handler]() {
      T message;
      reader_.read(&message);
      handler(std::move(message));
    });
  }

  template <typename T>
  void send(MessageTypeID type, const T& message) {
    std::ostringstream builder;
    StandardOutputStream output(builder);
    BinaryWriter(output).write<T>(message);
    sendBytes(type, builder.str());
  }

  // Wait for a message to be received and handle it.
  void poll();

  // Repeatedly handle messages until the socket closes.
  void serve();

  // Close the connection.
  void close();

  std::string hostPort() { return socket_.hostPort(); }

 private:
  void sendBytes(MessageTypeID type, const std::string& bytes);

  void on(MessageTypeID type, MessageHandlerBase handler);
  void unlockedPoll(std::unique_lock<std::mutex>& reader_lock);

  std::mutex reader_mutex_, writer_mutex_;

  Socket socket_;
  BinaryReader reader_;
  BinaryWriter writer_;

  std::unordered_map<MessageTypeID, MessageHandlerBase> handlers_;
};
