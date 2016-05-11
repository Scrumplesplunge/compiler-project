#include "config.h"

#include "../util.h"

#include <fstream>
#include <stdexcept>
#include <util/json.h>

using namespace std;

static void checkType(string id, JSON::Node node, JSON::Type expected_type) {
  if (node.type() != expected_type)
    throw runtime_error("Bad configuration: " + id + " is of the wrong type.");
}

static void checkMemberType(
    JSON::Object object, string key, JSON::Type expected_type) {
  if (object.count(key) == 0)
    throw runtime_error("Bad configuration: missing \"" + key + "\".");
  checkType("\"" + key + "\"", object.at(key), expected_type);
}

JobConfig loadConfig(string job_file) {
  // Load the config object.
  string contents = getFileContents(job_file);
  JSON::Node node = JSON::parse(contents);
  checkType("Root node", node, JSON::Type::OBJECT);
  JSON::Object& config = node.asObject();

  JobConfig out;

  checkMemberType(config, "name", JSON::Type::STRING);
  out.name = config["name"].asString();

  checkMemberType(config, "description", JSON::Type::STRING);
  out.description = config["description"].asString();

  checkMemberType(config, "bytecode_file", JSON::Type::STRING);
  out.bytecode_file = config["bytecode_file"].asString();

  checkMemberType(config, "metadata_file", JSON::Type::STRING);
  out.metadata_file = config["metadata_file"].asString();

  checkMemberType(config, "workers", JSON::Type::ARRAY);
  JSON::Array workers = config["workers"].asArray();

  if (workers.size() == 0)
    throw runtime_error("Bad configuration: No workers specified.");

  // Load all the workers.
  for (int i = 0, n = workers.size(); i < n; i++) {
    checkType("workers[" + to_string(i) + "]", workers[i], JSON::Type::OBJECT);
    JSON::Object& worker = workers[i].asObject();
    out.workers.push_back(
        WorkerAddress{worker["host"].asString(), worker["port"].asInt32()});
  }

  return out;
}
