#include "ChannelServer.h"

#include "../util.h"
#include "ProcessServer.h"

using namespace std;

static logic_error unhandled_transition(
    ChannelServerStateType state, ChannelServerEventType event) {
  return logic_error(
      "Unhandled transition " + ::toString(event) + " from state " +
      ::toString(state) + ".");
}

static runtime_error size_error(int32_t write_length, int32_t read_length) {
  return runtime_error(
      "Message size mismatch: " + to_string(write_length) +
      " byte(s) sent, but attempting to receive " + to_string(read_length) +
      " byte(s).");
}

bool ChannelServer::input(
    Instance* instance, int32_t workspace_descriptor,
    int32_t destination_address, int32_t length, Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : LOCAL_INPUT on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Reader arrived first.
      state.type = LOCAL_INPUT_WAIT;
      state.reader =
          Reader(instance, destination_address, length, workspace_descriptor);

      if (!state.is_local) {
        // Forward the message.
        MESSAGE(CHANNEL_INPUT) message;
        message.channel = channel;
        server_.send(message);
      }

      // Have to wait for writer.
      return false;
    case LOCAL_OUTPUT_WAIT:
      // Reader is satisfied by local writer.
      state.type = NORMAL;

      if (length != state.writer.length)
        throw size_error(state.writer.length, length);

      instance->transferBytesFrom(
          state.writer.instance, state.writer.source_address,
          destination_address, length);
      state.writer.instance->reschedule(state.writer.workspace_descriptor);

      if (!state.is_local) {
        // Resolve the communication.
        MESSAGE(CHANNEL_RESOLVED) message;
        message.channel = channel;
        server_.send(message);
      }

      // Communication is complete. No waiting required.
      return true;
    case REMOTE_OUTPUT_WAIT:
      // Reader is satisfied by remote writer.
      state.type = NORMAL;

      if (static_cast<int32_t>(state.writer.data.length()) != length)
        throw size_error(state.writer.data.length(), length);

      instance->importBytes(
          state.writer.data.c_str(), destination_address, length);

      {
        MESSAGE(CHANNEL_DONE) message;
        message.channel = channel;
        server_.send(message);
      }

      // Communication is complete. No waiting required.
      return true;
    default:
      throw unhandled_transition(state.type, LOCAL_INPUT);
  }
}

bool ChannelServer::output(
    Instance* instance, int32_t workspace_descriptor,
    int32_t source_address, int32_t length, Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : LOCAL_OUTPUT on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Writer arrived first.
      state.type = LOCAL_OUTPUT_WAIT;
      state.writer =
          Writer(instance, source_address, length, workspace_descriptor);

      if (!state.is_local) {
        // Forward the message.
        MESSAGE(CHANNEL_OUTPUT) message;
        message.channel = channel;
        message.data = instance->exportBytes(source_address, length);
        server_.send(message);
      }

      // Have to wait for reader.
      return false;
    case LOCAL_INPUT_WAIT:
      // Write matched with local reader.
      state.type = NORMAL;

      if (length != state.reader.length)
        throw size_error(length, state.reader.length);

      state.reader.instance->transferBytesFrom(
          instance, source_address, state.reader.destination_address, length);
      state.reader.instance->reschedule(state.reader.workspace_descriptor);

      if (!state.is_local) {
        MESSAGE(CHANNEL_RESOLVED) message;
        message.channel = channel;
        server_.send(message);
      }

      // Communication is complete. No need to wait.
      return true;
    case LOCAL_ENABLED:
    case REMOTE_ENABLED:
      // Channel is now ready.
      state.type = (state.type == LOCAL_ENABLED ? LOCAL_READY : REMOTE_READY);
     
      state.writer =
          Writer(instance, source_address, length, workspace_descriptor);

      // Have to wait for reader.
      return false;
    default:
      throw unhandled_transition(state.type, LOCAL_OUTPUT);
  }
}

bool ChannelServer::enable(
    Instance* instance, int32_t workspace_descriptor, Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : LOCAL_ENABLE on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // No writer is waiting.
      state.type = LOCAL_ENABLED;
      state.enabler = Enabler(instance, workspace_descriptor);
      return false;
    case LOCAL_OUTPUT_WAIT:
      // A local writer is already waiting.
      state.type = LOCAL_READY;
      state.enabler = Enabler(instance, workspace_descriptor);
      return true;
    case REMOTE_OUTPUT_WAIT:
      // A remote writer is already waiting.
      state.type = REMOTE_READY;
      state.enabler = Enabler(instance, workspace_descriptor);
      return true;
    default:
      throw unhandled_transition(state.type, LOCAL_OUTPUT);
  }
}

bool ChannelServer::disable(Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : LOCAL_DISABLE on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case LOCAL_ENABLED:
      // No writer is waiting.
      state.type = NORMAL;
      return false;
    case LOCAL_READY:
      // A writer is waiting.
      state.type =
        (state.writer.is_local ? LOCAL_OUTPUT_WAIT : REMOTE_OUTPUT_WAIT);
      return true;
    default:
      throw unhandled_transition(state.type, LOCAL_DISABLE);
  }
}

void ChannelServer::remoteInput(Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : REMOTE_INPUT on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Remote input has arrived first.
      state.type = REMOTE_INPUT_WAIT;
      state.reader = Reader();

      return;
    case LOCAL_OUTPUT_WAIT:
      // Writer is waiting. Forward the message.
      state.type = DONE_WAIT;
      
      {
        MESSAGE(CHANNEL_OUTPUT) message;
        message.channel = channel;
        message.data = state.writer.instance->exportBytes(
            state.writer.source_address, state.writer.length);
        server_.send(message);
      }

      return;
    default:
      throw unhandled_transition(state.type, REMOTE_INPUT);
  }
}

void ChannelServer::remoteOutput(Channel channel, std::string&& data) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : REMOTE_OUTPUT on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Remote output has arrived first.
      state.type = REMOTE_OUTPUT_WAIT;
      state.writer = Writer(move(data));

      return;
    case LOCAL_INPUT_WAIT:
      // Reader is waiting.
      state.type = NORMAL;

      if (static_cast<int32_t>(data.length()) != state.reader.length)
        throw size_error(data.length(), state.reader.length);

      state.reader.instance->importBytes(
          data.c_str(), state.reader.destination_address, state.reader.length);
      state.reader.instance->reschedule(state.reader.workspace_descriptor);

      {
        MESSAGE(CHANNEL_DONE) message;
        message.channel = channel;
        server_.send(message);
      }

      return;
    case LOCAL_ENABLED:
      // Local ALT is ready.
      state.type = LOCAL_READY;
      state.enabler.instance->altWake(state.enabler.workspace_descriptor);

      state.writer = Writer(move(data));

      return;
    default:
      throw unhandled_transition(state.type, REMOTE_OUTPUT);
  }
}

void ChannelServer::remoteEnable(Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : REMOTE_ENABLE on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // No writer is waiting.
      state.type = REMOTE_ENABLED;
      state.enabler = Enabler();
      return;
    case LOCAL_OUTPUT_WAIT:
      // A local writer is already waiting.
      state.type = LOCAL_READY;
      state.enabler = Enabler();

      {
        MESSAGE(CHANNEL_OUTPUT) message;
        message.channel = channel;
        message.data = state.writer.instance->exportBytes(
            state.writer.source_address, state.writer.length);
        server_.send(message);
      }

      return;
    default:
      throw unhandled_transition(state.type, REMOTE_ENABLE);
  }
}

void ChannelServer::remoteDisable(Channel channel) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(channel);

  verr << ::toString(state.type) << " : REMOTE_DISABLE on (" << channel.owner
       << ", " << addressString(channel.address) << ")\n";

  switch (state.type) {
    case REMOTE_ENABLED:
      // No writer is waiting.
      state.type = NORMAL;

      return;
    case REMOTE_READY:
      // A writer is waiting.
      state.type =
        (state.writer.is_local ? LOCAL_OUTPUT_WAIT : REMOTE_OUTPUT_WAIT);

      return;
    default:
      throw unhandled_transition(state.type, REMOTE_DISABLE);
  }
}

ChannelServer::ChannelState& ChannelServer::get(Channel channel) {
  if (channels_.count(channel) == 0) {
    ChannelState state;
    state.is_local = server_.hasInstance(channel.owner);
    channels_.emplace(channel, state);
  }

  return channels_.at(channel);
}
