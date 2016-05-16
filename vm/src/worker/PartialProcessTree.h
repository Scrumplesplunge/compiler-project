#pragma once

#include "../network.h"

#include <memory>
#include <mutex>
#include <unordered_map>

class PartialProcessTree {
 public:
  // Returns true if the specified instance is known by the tree. If this is a
  // non-local instance, then the instance is only guaranteed to remain present
  // until the last local descendent is gone.
  bool has(instance_id id);

  // Access information stored in the tree.
  InstanceInfo info(instance_id id);

  // Add a local instance and import the heirarchy specified by the given
  // ancestry. These will be discarded as soon as they are no longer referenced
  // by a local instance.  It is expected that if there is an instance in the
  // ancestry list, then the first is either a root instance or an instance
  // running on this ProcessServer.
  void addLocalInstance(InstanceInfo info, const Ancestry& ancestry);

  // Remove a local instance.
  void removeLocalInstance(instance_id id);

  // Compute the owning instance of a channel.
  Channel channel(instance_id actor, int32_t channel_address);

 private:
  struct InstanceInfoNode {
    InstanceInfoNode(InstanceInfo base);
    ~InstanceInfoNode();

    InstanceInfo info;
    std::shared_ptr<InstanceInfoNode> parent;
  };

  std::mutex mu_;

  std::unordered_map<instance_id, std::weak_ptr<InstanceInfoNode>> instances_;
  std::unordered_map<instance_id, std::shared_ptr<InstanceInfoNode>>
      local_instances_;
};
