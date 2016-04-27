#include "socket.h"

#include <arpa/inet.h>
#include <iostream>
#include <memory>
#include <netdb.h>
#include <stdexcept>
#include <string.h>
#include <unistd.h>

using namespace std;

static void throw_error(string message) {
  message += " (" + to_string(errno) + ")";
  string description = strerror(errno);
  if (description.length() > 0) message += ": " + description;
  throw runtime_error(message);
}

static void throw_getnameinfo_error(int result) {
  string message = "getnameinfo(): ";
  switch (result) {
    case EAI_AGAIN:    message += "EAI_AGAIN";    break;
    case EAI_BADFLAGS: message += "EAI_BADFLAGS"; break;
    case EAI_FAIL:     message += "EAI_FAIL";     break;
    case EAI_FAMILY:   message += "EAI_FAMILY";   break;
    case EAI_MEMORY:   message += "EAI_MEMORY";   break;
    case EAI_NONAME:   message += "EAI_NONAME";   break;
    case EAI_OVERFLOW: message += "EAI_OVERFLOW"; break;
    case EAI_SYSTEM:
      message = string("Failed to fetch host and port information: ") +
                strerror(errno);
      break;
  }
  throw runtime_error(message);
}

Socket::Socket(Socket&& source)
    : socket_(source.socket_),
      port_(source.port_),
      host_(source.host_),
      queue_size_(source.queue_size_) {
  source.socket_ = -1;
}

void Socket::connect(string host, int port) { connect(host, to_string(port)); }

void Socket::connect(string host, string service) {
  sockaddr_in address = initSocket(host, service);

  // Establish the connection.
  int result = ::connect(socket_, reinterpret_cast<sockaddr*>(&address),
                         sizeof(address));
  if (result < 0) throw_error("Could not connect to '" + hostPort() + "'");
}

void Socket::bind(string host, int port) { return bind(host, to_string(port)); }

void Socket::bind(string host, string service) {
  sockaddr_in address = initSocket(host, service);

  // Bind the address to the socket.
  int result = ::bind(socket_, reinterpret_cast<sockaddr*>(&address),
                      sizeof(address));
  if (result < 0) throw_error("Could not bind to '" + hostPort() + "'");
}

void Socket::listen() {
  int result = ::listen(socket_, queue_size_);
  if (result < 0) throw_error("Failed to listen on socket");
}

Socket Socket::accept() {
  // Build a new Socket instance to accept into.
  Socket out;
  sockaddr_in address;
  socklen_t length = sizeof(address);

  // Accept the incoming connection.
  out.socket_ = ::accept(socket_, reinterpret_cast<sockaddr*>(&address),
                         &length);
  if (out.socket_ < 0) throw_error("Failed whilst accepting connection");

  // Finish setting up the Socket instance.
  out.configureHostPort(address);
  return out;
}

void Socket::send(string data) {
  send(data.c_str(), data.length());
}

void Socket::send(const char* buffer, int length) {
  int result = ::send(socket_, buffer, length, MSG_NOSIGNAL);
  if (result < 0) throw_error("Failed to send " + to_string(length) + " bytes");
}

string Socket::receive(int limit) {
  // Construct a temporary buffer to receive the data into.
  unique_ptr<char[]> buffer(new char[limit]);
  int length = receive(buffer.get(), limit);
  return string(buffer.get(), length);
}

int Socket::receive(char* buffer, int limit) {
  int length = ::recv(socket_, buffer, limit, 0);
  if (length < 0)
    throw_error("Got status " + to_string(length) + " on receive");
  return length;
}

void Socket::shutdown() {
  if (socket_ >= 0) {
    ::shutdown(socket_, SHUT_RDWR);
    close(socket_);
  }
}

bool Socket::get(char& c) {
  int result = ::recv(socket_, &c, 1, 0);
  return result > 0;
}

int64_t Socket::read(char* buffer, int64_t length) {
  int64_t received = 0;
  while (received < length) {
    int64_t result = ::recv(socket_, buffer + received, length - received, 0);
    if (result < 0) return received;
    received += result;
  }
  return length;
}

void Socket::closeInput() { ::shutdown(socket_, SHUT_RD); }

bool Socket::put(char c) {
  int result = ::send(socket_, &c, 1, 0);
  return result > 0;
}

int64_t Socket::write(const char* buffer, int64_t length) {
  int64_t result = ::send(socket_, buffer, length, MSG_NOSIGNAL);
  if (result < 0) return 0;
  return result;
}

void Socket::closeOutput() { ::shutdown(socket_, SHUT_WR); }

sockaddr_in Socket::initSocket(string host, string service) {
  shutdown();

  // Open the socket file descriptor.
  socket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_ < 0) throw_error("Failed to open socket");

  // Look up the address.
  sockaddr_in addr = getSocketAddress(host, service);
  configureHostPort(addr);

  return addr;
}

sockaddr_in Socket::getSocketAddress(std::string host, std::string service) {
  // Socket hints.
  addrinfo hints = {0};
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  addrinfo* list = nullptr;

  // Look up the host.
  int result = ::getaddrinfo(host.c_str(), service.c_str(), &hints, &list);
  if (result != 0) {
    throw_error("Failed to get address information with result " +
                to_string(result));
  } else if (list == nullptr) {
    throw_error("No results returned when fetching address information");
  }

  // Return the first result.
  sockaddr_in address;
  *reinterpret_cast<sockaddr*>(&address) = *list->ai_addr;

  freeaddrinfo(list);
  return address;
}

void Socket::configureHostPort(const sockaddr_in& address) {
  // Retrieve the host and port from the socket address.
  const int HOST_NAME_LENGTH = 256;
  char host_name[HOST_NAME_LENGTH];
  int result = ::getnameinfo(reinterpret_cast<const sockaddr*>(&address),
                             sizeof(address), host_name, HOST_NAME_LENGTH,
                             nullptr, 0, 0);

  if (result != 0) throw_getnameinfo_error(result);

  host_ = string(host_name);
  port_ = ntohs(address.sin_port);
}
