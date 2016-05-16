#include "PartialProcessTree.h"

#include <util/atomic_output.h>

using namespace std;

bool PartialProcessTree::has(instance_id id) {
  unique_lock<mutex> lock(mu_);

  if (instances_.count(id) == 0) return false;

  if (instances_.at(id).expired()) {
    // Instance was present, but has been discarded.
    instances_.erase(id);
    return false;
  } else {
    return true;
  }
}

InstanceInfo PartialProcessTree::info(instance_id id) {
  unique_lock<mutex> lock(mu_);

  // Acquire a pointer to the instance.
  if (instances_.count(id) == 0)
    throw logic_error("Tried to access unknown instance.");
  shared_ptr<InstanceInfoNode> instance = instances_.at(id).lock();
  if (!instance) throw logic_error("Instance has been discarded.");

  return instance->info;
}

void PartialProcessTree::addLocalInstance(
    InstanceInfo info, const Ancestry& ancestry) {
  info.id = ancestry.subject;
  if (ancestry.contents.size() > 0) {
    // Instance has a parent.
    info.parent_id = ancestry.contents.back().id;
  } else {
    // Instance is a root node.
    info.parent_id = info.id;
  }

  if (instances_.count(info.id) > 0)
    throw logic_error("Adding instance that already exists.");

  // Create this instance.
  auto instance = make_shared<InstanceInfoNode>(info);
  instances_.emplace(info.id, instance);
  local_instances_.emplace(info.id, instance);

  if (ancestry.contents.size() == 0) return;

  shared_ptr<InstanceInfoNode> node;

  // Create all ancestors.
  for (const InstanceInfo& info : ancestry.contents) {
    // Check that the node does not already exist.
    if (instances_.count(info.id) && !instances_.at(info.id).expired())
      continue;

    // Construct this instance.
    auto temp = make_shared<InstanceInfoNode>(info);
    if (info.id != info.parent_id) {
      if (!instances_.count(info.parent_id))
        throw logic_error("Parent instance of ancestor does not exist.");
      shared_ptr<InstanceInfoNode> parent =
          instances_.at(info.parent_id).lock();
      if (!parent)
        throw logic_error("Parent instance of ancestor has been discarded.");
      temp->parent = move(parent);
    }

    // Place it into the instances list, keeping a shared_ptr so as not to let
    // it be discarded.
    instances_.emplace(info.id, temp);
    node = move(temp);
  }

  // Attach the last instance to the instance subject.
  instance->parent = move(node);
}

void PartialProcessTree::removeLocalInstance(instance_id id) {
  if (local_instances_.count(id) == 0)
    throw logic_error("Removing instance that does not exist.");
  local_instances_.erase(id);
}

Channel PartialProcessTree::channel(
    instance_id actor, int32_t channel_address) {
  unique_lock<mutex> lock(mu_);

  if (local_instances_.count(actor) == 0)
    throw logic_error("Channel use by unknown/non-local instance.");
  InstanceInfoNode* node = local_instances_.at(actor).get();

  // Loop until the channel is found.
  while (true) {
    if (node->info.memory_start <= channel_address &&
        channel_address < node->info.memory_end) {
      // Owner discovered.
      return Channel{node->info.id, channel_address};
    }

    if (node->info.id == node->info.parent_id)
      throw logic_error("Instance used channel with unreachable address.");

    node = node->parent.get();
  }
}

PartialProcessTree::InstanceInfoNode::InstanceInfoNode(InstanceInfo base)
    : info(base) {
  verr << "Node " << info.id << " created.\n";
}

PartialProcessTree::InstanceInfoNode::~InstanceInfoNode() {
  verr << "Node " << info.id << " discarded.\n";
}
