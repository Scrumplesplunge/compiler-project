#pragma once

#include "stream.h"

#include <netinet/in.h>
#include <stdexcept>
#include <string>
#include <sys/socket.h>

class socket_error : public std::runtime_error {
 public:
  socket_error(std::string message)
      : std::runtime_error(message) {}
};

// Low-level wrapper around the socket interface.
class Socket : public InputStream, public OutputStream {
 public:
  Socket() = default;
  Socket(Socket&& source);

  // Client.
  void connect(std::string host, int port);
  void connect(std::string host, std::string service);

  // Server.
  void bind(std::string host, int port);
  void bind(std::string host, std::string service);
  void listen();
  Socket accept();

  // Communication.
  void send(std::string data);
  void send(const char* buffer, int length);
  
  std::string receive(int limit);
  int receive(char* buffer, int limit);

  // Termination.
  void shutdown();

  // Configuration.
  int queue_size() const { return queue_size_; }
  void set_queue_size(int queue_size) { queue_size_ = queue_size; }

  // Status information.
  std::string host() const { return host_; }
  int port() const { return port_; }
  std::string hostPort() const { return host_ + ":" + std::to_string(port_); }

  // InputStream interface. See stream.h for descriptions.
  bool get(char& c) override;
  int64_t read(char* buffer, int64_t length) override;
  void closeInput() override;

  // OutputStream interface. See stream.h for descriptions.
  bool put(char c) override;
  int64_t write(const char* buffer, int64_t length) override;
  void closeOutput() override;

 private:
  sockaddr_in initSocket(std::string host, std::string service);
  sockaddr_in getSocketAddress(std::string host, std::string service);
  void configureHostPort(const sockaddr_in& address);

  // Socket state.
  int socket_ = -1;
  int port_ = 0;
  std::string host_ = "";

  // Configuration.
  int queue_size_ = 8;
};
