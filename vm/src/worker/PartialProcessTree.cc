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

void PartialProcessTree::import(const Ancestry& ancestry) {
  unique_lock<mutex> lock(mu_);

  // Look up the subject instance.
  shared_ptr<InstanceInfoNode> instance =
      instances_.at(ancestry.subject).lock();
  if (!instance)
    throw logic_error("Importing ancestry for non-existent instance.");

  shared_ptr<InstanceInfoNode> previous =
    instances_.at(ancestry.contents.front().parent_id).lock();
  if (!previous) throw logic_error("Oldest instance in ancestry is an orphan.");

  for (const InstanceInfo& info : ancestry.contents) {
    // Construct this instance.
    auto instance = make_shared<InstanceInfoNode>(info);
    if (info.id != info.parent_id) instance->parent = move(previous);

    // Place it into the instances list, keeping a shared_ptr so as not to let
    // it be discarded.
    instances_.emplace(info.id, instance);
    previous = move(instance);
  }

  // Attach the last instance to the instance subject.
  instance->parent = move(previous);
}

void PartialProcessTree::addLocalInstance(InstanceInfo info) {
  if (instances_.count(info.id) > 0)
    throw logic_error("Adding instance that already exists.");

  auto instance = make_shared<InstanceInfoNode>(info);
  instances_.emplace(info.id, instance);
  local_instances_.emplace(info.id, move(instance));
}

void PartialProcessTree::removeLocalInstance(instance_id id) {
  if (local_instances_.count(id) == 0)
    throw logic_error("Removing instance that does not exist.");
  local_instances_.erase(id);
}

PartialProcessTree::InstanceInfoNode::InstanceInfoNode(InstanceInfo base)
    : info(base) {
  verr << "Node " << info.id << " created.\n";
}

PartialProcessTree::InstanceInfoNode::~InstanceInfoNode() {
  verr << "Node " << info.id << " discarded.\n";
}
