#pragma once

#include "../network.h"

#include <condition_variable>
#include <mutex>
#include <unordered_map>

class ProcessTree {
 public:
  // Create a new root instance. The InstanceInfo can leave id and parent_id
  // undefined. They will both be set to the value which is returned.
  instance_id createRootInstance(InstanceInfo info);

  // Create a new child instance. The InstanceInfo can leave id undefined. It
  // will be set to the value which is returned.
  instance_id createInstance(InstanceInfo info);

  // Check whether an instance is still alive (assuming that it ever was).
  bool is_active(instance_id id);

  // Look up the information for a particular instance.
  InstanceInfo info(instance_id id);

  // Follow the ancestry for the given instance until one is found which is
  // running on the given worker. This is used for sharing the process tree. The
  // result is an ancestry which contains all parents between the given instance
  // and the first parent which is hosted on the specified worker (neither of
  // which is included). If no such parent exists, then the ancestry will lead
  // all the way up to the root instance.
  Ancestry link(instance_id id, worker_id worker);

  // End the instance specified by the given ID. If the instance still has
  // active children, this will throw an exception.
  void endInstance(instance_id id);

  // Join on an instance ending. If the instance does not exist, it is assumed
  // to have finished and the call will return immediately.
  void join(instance_id id);

 private:
  struct InstanceInfoNode {
    InstanceInfoNode(InstanceInfo base)
        : info(base) {}

    int num_children = 0;
    InstanceInfo info;

    bool done = false;
    std::condition_variable on_done;
  };

  instance_id next_id_ = 1;
  std::mutex exit_mu_;
  std::unordered_map<instance_id, std::shared_ptr<InstanceInfoNode>> instances_;
};
