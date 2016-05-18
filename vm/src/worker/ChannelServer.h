#pragma once

#include "../network.h"
#include "Instance.h"
#include "worker/ChannelServerEventType.h"
#include "worker/ChannelServerStateType.h"

#include <mutex>
#include <string>
#include <unordered_map>

class ProcessServer;

class ChannelServer {
 public:
  ChannelServer(ProcessServer& server)
      : server_(server) {}

  // Called by instances. A return value of true means that the read or write
  // happened immediately. A return value of false means that the thread should
  // deschedule itself.
  bool input(
      Instance* instance, int32_t workspace_descriptor,
      int32_t destination_address, int32_t length, Channel channel);
  bool output(Instance* instance, int32_t workspace_descriptor,
              int32_t source_address, int32_t length, Channel channel);

  // These return true if a writer is already waiting.
  bool enable(
      Instance* instance, int32_t workspace_descriptor, Channel channel);
  bool disable(Channel channel);

  // Called by ProcessServer for incoming messages.
  void onInput(MESSAGE(CHANNEL_INPUT)&& message);
  void onOutput(MESSAGE(CHANNEL_OUTPUT)&& message);
  void onEnable(MESSAGE(CHANNEL_ENABLE)&& message);
  void onDisable(MESSAGE(CHANNEL_DISABLE)&& message);

  void onResolved(MESSAGE(CHANNEL_RESOLVED)&& message);
  void onDone(MESSAGE(CHANNEL_DONE)&& message);

 private:
  struct ChannelState;

  ChannelState& get(Channel channel);

  struct Enabler {
    Enabler() : is_local(false) {}
    Enabler(Instance* i, int32_t w)
        : is_local(true), instance(i), workspace_descriptor(w) {}

    bool is_local;
    Instance* instance;
    int32_t workspace_descriptor;
  };

  struct Reader {
    Reader() : is_local(false) {}
    Reader(Instance* i, int32_t d, int32_t l, int32_t w)
        : is_local(true), instance(i), destination_address(d), length(l),
          workspace_descriptor(w) {}

    bool is_local;

    Instance* instance;
    int32_t destination_address, length;
    int32_t workspace_descriptor;
  };

  struct Writer {
    Writer() {}

    Writer(Instance* i, int32_t s, int32_t l, int32_t w)
        : is_local(true), length(l), instance(i), source_address(s),
          workspace_descriptor(w) {}

    Writer(std::string&& d)
        : is_local(false), length(d.length()), data(move(d)) {}

    bool is_local;
    int32_t length;
    
    // Contents if is_local is true.
    Instance* instance;
    int32_t source_address;
    int32_t workspace_descriptor;

    // Contents if is_local is false.
    std::string data;
  };

  struct ChannelState {
    ChannelState() {}

    ChannelServerStateType type = NORMAL;

    bool is_local;

    union {
      Enabler enabler;
      Reader reader;
    };

    Writer writer;
  };

  std::mutex mu_;

  ProcessServer& server_;
  std::unordered_map<Channel, ChannelState, ChannelHasher> channels_;
};
