#pragma once

#include "../network.h"

#include <condition_variable>
#include <list>
#include <memory>
#include <mutex>
#include <stdint.h>
#include <unordered_map>
#include <util/binary.h>

struct InstanceInfo {
  instance_id id, parent_id;
  worker_id location;
};

struct Ancestry {
  // Ancestors should be sorted such that the first element is a root node, and
  // every subsequent entry is a direct child of the previous, the last entry
  // being the subject of the ancestry. This means that if the subject is a root
  // node, it will be the only entry.
  //
  // Note that since instance IDs only increase, and sub-instances must be
  // created after parent instances, this is ordered by increasing instance IDs.
  std::list<InstanceInfo> ancestors;
};

template <> void BinaryReader::read(Ancestry* ancestry);
template <> void BinaryWriter::write(const Ancestry& ancestry);

class ProcessTree {
 public:
  instance_id createRootInstance(worker_id location);
  instance_id createInstance(instance_id parent, worker_id location);

  // Check whether an instance is still alive (assuming that it ever was).
  bool is_active(instance_id id);

  // Look up the information for a particular instance.
  const InstanceInfo& info(instance_id id);

  // Look up the ancestry for a process.
  Ancestry ancestors(instance_id id);

  // End the instance specified by the given ID. If the instance still has
  // active children, this will throw an exception.
  void endInstance(instance_id id);

  // Join on an instance ending. If the instance does not exist, it is assumed
  // to have finished and the call will return immediately.
  void join(instance_id id);

 private:
  struct InstanceInfoNode {
    InstanceInfoNode(InstanceInfo&& base)
        : info(base) {}

    int num_children = 0;
    InstanceInfo info;

    bool done = false;
    std::condition_variable on_done;
  };

  std::mutex mu_;

  instance_id next_id_ = 1;
  std::unordered_map<instance_id, std::shared_ptr<InstanceInfoNode>> instances_;
};
