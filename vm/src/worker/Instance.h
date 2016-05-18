#pragma once

#include "../network.h"
#include "../runtime/VM.h"

#include <condition_variable>

class ProcessServer;

class Instance : public VM {
 public:
  Instance(ProcessServer& server, instance_id id,
           const InstanceDescriptor& descriptor, const char* bytecode,
           int32_t bytecode_size, const int32_t* static_data,
           int32_t static_data_size);
  virtual ~Instance() = default;

  instance_id id() const { return id_; }

  void reschedule(int32_t workspace_descriptor);
  void altWake(int32_t workspace_descriptor);
  void childStarted(int32_t handle_address, instance_id id);

  std::string toString() override;

 protected:
  friend class ProcessServer;

  void runSpecialInstruction(Indirect op) override;
  void onEmptyProcessQueue(std::unique_lock<std::mutex>& lock) override;

  void wake(int32_t workspace_descriptor);

  void startInstance();
  void joinInstance();

  #define VIRTUAL(type) void indirect_##type() override
  VIRTUAL(ALT);  VIRTUAL(ALTEND); VIRTUAL(ALTWT); VIRTUAL(DISC);
  VIRTUAL(ENBC); VIRTUAL(IN);     VIRTUAL(OUT);
  #undef VIRTUAL

 private:
  std::condition_variable on_wake_;

  struct ChildHandle {
    enum State { NO_ID, ID, JOIN_NO_ID };
    State state = NO_ID;
    instance_id id;
    int32_t workspace_descriptor;
  };

  // These are protected by queue_mu_.
  std::unordered_map<int32_t, ChildHandle> children_;
  int32_t waiting_processes_ = 0;

  const instance_id id_;
  ProcessServer& server_;
};
