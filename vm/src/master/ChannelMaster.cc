#include "ChannelMaster.h"

#include "../util.h"
#include "ProcessMaster.h"

using namespace std;

static logic_error unhandled_transition(
    ChannelMasterStateType state, Network event) {
  return logic_error(
      "Unhandled transition " + ::toString(event) + " from state " +
      ::toString(state) + ".");
}

void ChannelMaster::onInput(
    worker_id worker, MESSAGE(CHANNEL_INPUT)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Reader arrived first.
      state.type = INPUT_WAIT;
      if (worker == state.owner_worker)
        throw logic_error("Should not receive input from owner.");
      master_.workers_[state.owner_worker]->send(message);
      state.reader = Reader{worker};
      return;
    case OUTPUT_WAIT:
      // Writer is already waiting.
      state.type = DONE_WAIT;
      {
        // Forward the output.
        MESSAGE(CHANNEL_OUTPUT) forward;
        forward.channel = message.channel;
        forward.data = state.writer.data;
        master_.workers_[state.reader.worker]->send(forward);
      }
      {
        // Inform the owner.
        MESSAGE(CHANNEL_RESOLVED) resolve;
        resolve.channel = message.channel;
        master_.workers_[state.owner_worker]->send(resolve);
      }
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_INPUT);
  }
}

void ChannelMaster::onOutput(
    worker_id worker, MESSAGE(CHANNEL_OUTPUT)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Writer arrived first.
      state.type = OUTPUT_WAIT;
      if (worker == state.owner_worker)
        throw logic_error("Should not receive unrequested output from owner.");
      master_.workers_[state.owner_worker]->send(message);
      state.writer = Writer{worker, move(message.data)};
      return;
    case INPUT_WAIT:
      // Reader is already waiting. Forward the output.
      state.type = DONE_WAIT;
      master_.workers_[state.reader.worker]->send(message);
      if (worker != state.owner_worker) {
        // Inform the owner.
        MESSAGE(CHANNEL_RESOLVED) resolve;
        resolve.channel = message.channel;
        master_.workers_[state.owner_worker]->send(resolve);
      }
      state.writer = Writer{worker, move(message.data)};
      return;
    case ENABLED:
      // Reader is waiting in an ALT. Forward the output. The channel may not be
      // chosen by the alternative, but since the exclusivity of channels is a
      // requirement of the language, only this reader may subsequently read the
      // value.
      state.type = READY;
      master_.workers_[state.enabler.worker]->send(message);
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_OUTPUT);
  }
}

void ChannelMaster::onEnable(
    worker_id worker, MESSAGE(CHANNEL_ENABLE)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case NORMAL:
      // Enabler arrived first.
      state.type = ENABLED;
      state.enabler = Enabler{worker};
      if (worker == state.owner_worker)
        throw logic_error("Should not receive enable from owner.");
      master_.workers_[state.owner_worker]->send(message);
      return;
    case OUTPUT_WAIT:
      // Writer is waiting. Forward the output. See the description for ENABLED
      // in onOutput above for why this is correct.
      state.type = READY;
      if (worker == state.owner_worker)
        throw logic_error("Should not receive enable from owner.");
      {
        // Forward the output.
        MESSAGE(CHANNEL_OUTPUT) forward;
        forward.channel = message.channel;
        forward.data = state.writer.data;
        master_.workers_[worker]->send(forward);
      }
      {
        // Inform the owner.
        MESSAGE(CHANNEL_RESOLVED) resolve;
        resolve.channel = message.channel;
        master_.workers_[state.owner_worker]->send(resolve);
      }
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_ENABLE);
  }
}

void ChannelMaster::onDisable(
    worker_id worker, MESSAGE(CHANNEL_DISABLE)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case ENABLED:
      // No writer arrived.
      state.type = NORMAL;
      if (worker == state.owner_worker)
        throw logic_error("Should not receive disable from owner.");
      master_.workers_[state.owner_worker]->send(message);
      return;
    case READY:
      // Writer arrived (and has been forwarded).
      state.type = OUTPUT_WAIT;
      if (worker == state.owner_worker)
        throw logic_error("Should not receive disable from owner.");
      master_.workers_[state.owner_worker]->send(message);
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_DISABLE);
  }
}

void ChannelMaster::onResolved(
    worker_id worker, MESSAGE(CHANNEL_RESOLVED)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case INPUT_WAIT:
    case OUTPUT_WAIT:
      // Communication has been externally resolved.
      state.type = NORMAL;
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_RESOLVED);
  }
}

void ChannelMaster::onDone(
    worker_id worker, MESSAGE(CHANNEL_DONE)&& message) {
  unique_lock<mutex> lock(mu_);
  ChannelState& state = get(message.channel);

  verr << ::toString(state.type) << " : " << ::toString(message.type)
       << " on (" << message.channel.owner << ", "
       << addressString(message.channel.address) << ")\n";

  switch (state.type) {
    case OUTPUT_WAIT:
    case DONE_WAIT:
      // Communication has completed.
      state.type = NORMAL;
      master_.workers_[state.writer.worker]->send(message);
      return;
    default:
      throw unhandled_transition(state.type, CHANNEL_DONE);
  }
}

ChannelMaster::ChannelState& ChannelMaster::get(Channel channel) {
  if (channels_.count(channel) == 0) {
    ChannelState state;
    state.owner_worker =
        master_.process_tree_.info(channel.owner).location;
    channels_.emplace(channel, state);
  }

  return channels_.at(channel);
}
