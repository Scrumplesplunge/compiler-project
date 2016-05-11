#include "ProcessTree.h"

using namespace std;

template <>
void BinaryReader::read(Ancestry* ancestry) {
  ancestry->ancestors.clear();

  uint64_t num_entries = readVarUint();

  bool first = true;
  InstanceInfo temp;
  instance_id previous;
  for (uint64_t i = 0; i < num_entries; i++) {
    if (first) {
      // Read the (non delta-encoded) instance ID.
      temp.parent_id = previous = readVarUint();
      first = false;
    } else {
      temp.parent_id = previous;
      previous += readVarUint();
    }
    temp.id = previous;
    // Read the worker ID.
    temp.location = readVarUint();

    ancestry->ancestors.push_back(temp);
  }
}

template <>
void BinaryWriter::write(const Ancestry& ancestry) {
  writeVarUint(ancestry.ancestors.size());

  instance_id previous = 0;
  for (const InstanceInfo& instance : ancestry.ancestors) {
    if (instance.id < previous)
      throw logic_error("Instance ID values are not increasing.");
    // Delta-encode the instance ID.
    writeVarUint(instance.id - previous);
    previous = instance.id;

    // Write the worker ID.
    writeVarUint(instance.location);
  }
}

instance_id ProcessTree::createRootInstance(worker_id location) {
  unique_lock<mutex> lock(mu_);

  // Root nodes are their own parent.
  instance_id id = next_id_++;
  auto instance = make_shared<InstanceInfoNode>(InstanceInfo{id, id, location});
  instances_.emplace(id, instance);
  return id;
}

instance_id ProcessTree::createInstance(
    instance_id parent_id, worker_id location) {
  unique_lock<mutex> lock(mu_);

  // Find the parent instance.
  auto i = instances_.find(parent_id);
  if (i == instances_.end())
    throw logic_error("Tried to create orphan instance.");
  i->second->num_children++;
  instance_id id = next_id_++;
  auto instance =
      make_shared<InstanceInfoNode>(InstanceInfo{id, parent_id, location});
  instances_.emplace(id, instance);
  return id;
}

bool ProcessTree::is_active(instance_id id) {
  unique_lock<mutex> lock(mu_);

  return instances_.count(id) > 0;
}

const InstanceInfo& ProcessTree::info(instance_id id) {
  unique_lock<mutex> lock(mu_);

  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end())
    throw logic_error("Looked up non-existent instance.");

  return i->second->info;
}

Ancestry ProcessTree::ancestors(instance_id id) {
  unique_lock<mutex> lock(mu_);

  Ancestry out;
  // Find the instance.
  auto i = instances_.find(id);
  
  do {
    if (i == instances_.end())
      throw logic_error("Instance is orphaned or does not exist.");
    InstanceInfoNode& node = *i->second;

    // Add the node to the list.
    out.ancestors.push_front(node.info);

    // Move to the parent.
    id = node.info.parent_id;
  } while (i->second->info.id != i->second->info.parent_id);

  return out;
}

void ProcessTree::endInstance(instance_id id) {
  unique_lock<mutex> lock(mu_);

  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end())
    throw logic_error("Tried to end non-existent instance.");

  InstanceInfoNode& node = *i->second;

  if (node.num_children > 0) {
    throw logic_error("Instance ending before " +
                      to_string(node.num_children) + " child(ren).");
  }

  if (node.info.parent_id != node.info.id) {
    // Find the parent.
    auto j = instances_.find(node.info.parent_id);
    if (j == instances_.end())
      throw logic_error("Orphaned instance.");

    // Decrement the child count.
    j->second->num_children--;
  }

  // Wake up the joining process(es).
  node.done = true;
  node.on_done.notify_all();
  instances_.erase(i);
}

void ProcessTree::join(instance_id id) {
  unique_lock<mutex> lock(mu_);

  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end()) return;

  // Wait for it to terminate.
  auto node = i->second;
  while (!node->done) node->on_done.wait(lock);
}
