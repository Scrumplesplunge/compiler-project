// In network.h:
DECLARE_MESSAGE(START_PROCESS_SERVER) {
  // This information is shown in the verbose output on the process server.
  std::string name;
  std::string description;

  std::string data;
  std::string bytecode;

  worker_id id;
};

// In network.cc:
DEFINE_MESSAGE(START_PROCESS_SERVER);

READER(START_PROCESS_SERVER) {
  message->name = readString();
  message->description = readString();
  message->data = readString();
  message->bytecode = readString();
  message->id = readVarUint();
}

WRITER(START_PROCESS_SERVER) {
  writeString(message.name);
  writeString(message.description);
  writeString(message.data);
  writeString(message.bytecode);
  writeVarUint(message.id);
}
