#include "metadata.h"

#include "../util.h"

#include <fstream>
#include <stdexcept>
#include <util/json.h>

using namespace std;

static void checkType(string id, JSON::Node node, JSON::Type expected_type) {
  if (node.type() != expected_type)
    throw runtime_error("Bad metadata file: " + id + " is of the wrong type.");
}

static void checkMemberType(
    JSON::Object object, string key, JSON::Type expected_type) {
  if (object.count(key) == 0)
    throw runtime_error("Bad metadata file: missing \"" + key + "\".");
  checkType("\"" + key + "\"", object.at(key), expected_type);
}

MetaData loadMetaData(string metadata_file) {
  // Load the metadata object.
  string contents = getFileContents(metadata_file);
  JSON::Node node = JSON::parse(contents);
  checkType("Root node", node, JSON::Type::OBJECT);
  JSON::Object& metadata = node.asObject();

  MetaData out;

  // Load the basic fields.
  checkMemberType(metadata, "memory_start", JSON::Type::INT);
  out.memory_start = metadata["memory_start"].asInt32();

  checkMemberType(metadata, "workspace_pointer", JSON::Type::INT);
  out.workspace_pointer = metadata["workspace_pointer"].asInt32();

  checkMemberType(metadata, "memory_size", JSON::Type::INT);
  out.memory_size = metadata["memory_size"].asInt32();

  checkMemberType(metadata, "assembly_file", JSON::Type::STRING);
  out.assembly_file = metadata["assembly_file"].asString();

  // Load and convert the static data.
  checkMemberType(metadata, "static_data", JSON::Type::ARRAY);
  JSON::Array& static_data = metadata["static_data"].asArray();

  for (int i = 0, n = static_data.size(); i < n; i++) {
    checkType("static_data[" + to_string(i) + "]", static_data[i],
              JSON::Type::INT);
    int value = static_data[i].asInt32();
    if (value < 0 || 256 <= value) {
      throw runtime_error(
          "Bad metadata file: static data contains non-byte value(s).");
    }
    out.static_data.push_back(static_cast<char>(value));
  }

  return out;
}
