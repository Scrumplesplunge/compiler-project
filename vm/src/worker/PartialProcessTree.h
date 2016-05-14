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

  // Import the heirarchy specified by the given ancestry. These will be
  // discarded as soon as they are no longer referenced by a local instance.
  // It is expected that if there is an instance in the ancestry list, then the
  // first is either a root instance or an instance running on this
  // ProcessServer.
  void import(const Ancestry& ancestry);

  // Add a local instance. These will not be automatically discarded.
  void addLocalInstance(InstanceInfo info);

  // Remove a local instance.
  void removeLocalInstance(instance_id id);

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
