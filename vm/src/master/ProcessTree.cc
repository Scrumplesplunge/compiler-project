#include "ProcessTree.h"

#include <util/atomic_output.h>

using namespace std;

instance_id ProcessTree::createRootInstance(InstanceInfo info) {
  // Root nodes are their own parent.
  instance_id id = info.id = info.parent_id = next_id_++;

  auto instance = make_shared<InstanceInfoNode>(info);
  instances_.emplace(id, instance);
  return id;
}

instance_id ProcessTree::createInstance(InstanceInfo info) {
  // Find the parent instance.
  auto i = instances_.find(info.parent_id);
  if (i == instances_.end()) {
    throw logic_error("Tried to create orphan instance: no such parent " +
                      to_string(info.parent_id));
  }

  // Increment the child count.
  i->second->num_children++;

  // Create the InstanceInfoNode.
  instance_id id = info.id = next_id_++;
  auto instance = make_shared<InstanceInfoNode>(info);
  instances_.emplace(id, instance);
  return id;
}

bool ProcessTree::is_active(instance_id id) {
  return instances_.count(id) > 0;
}

InstanceInfo ProcessTree::info(instance_id id) {
  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end())
    throw logic_error("Looked up non-existent instance: " + to_string(id));

  return i->second->info;
}

Ancestry ProcessTree::link(instance_id id, worker_id worker) {
  Ancestry out;
  out.subject = id;

  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end()) {
    throw logic_error(
        "Instance " + to_string(id) + " is orphaned or does not exist.");
  }
  InstanceInfo* info = &i->second->info;

  // Until either a root node is reached, or until the worker is discovered in
  // the ancestry if the instance, keep appending parents.
  do {
    i = instances_.find(info->parent_id);
    if (i == instances_.end()) {
      throw logic_error("Instance " + to_string(info->id) +
                        " is orphaned or does not exist.");
    }
    info = &i->second->info;

    out.contents.push_front(*info);
  } while (info->id != info->parent_id && info->location != worker);

  return out;
}

void ProcessTree::endInstance(instance_id id) {
  verr << "ENDING INSTANCE " << id << "\n";

  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end())
    throw logic_error("Tried to end non-existent instance " + to_string(id));

  InstanceInfoNode& node = *i->second;

  if (node.num_children > 0) {
    throw logic_error("Instance " + to_string(id) + " ending before " +
                      to_string(node.num_children) + " child(ren).");
  }

  if (node.info.parent_id != node.info.id) {
    // Find the parent.
    auto j = instances_.find(node.info.parent_id);
    if (j == instances_.end()) {
      throw logic_error(
          "Instance " + to_string(node.info.id) + " is an orphan.");
    }

    // Decrement the child count.
    j->second->num_children--;
  }

  // Wake up the joining process(es).
  unique_lock<mutex> lock(exit_mu_);
  node.done = true;
  node.on_done.notify_all();
  instances_.erase(i);
}

void ProcessTree::join(instance_id id) {
  // Find the instance.
  auto i = instances_.find(id);
  if (i == instances_.end()) return;

  // Wait for it to terminate.
  auto node = i->second;

  unique_lock<mutex> lock(exit_mu_);
  while (!node->done) node->on_done.wait(lock);
}
