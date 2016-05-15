#include "channel_info.h"

size_t ChannelHasher::operator()(const Channel& channel) const {
  return channel.owner ^ channel.address;
}
