#include "util/args.h"
#include "util/messenger.h"

#include <iostream>
#include <string>

using namespace std;

OPTION(string, host, "0.0.0.0", "Host to connect to.");
OPTION(int, port, 12345, "Port to connect to.");

FLAG(receiver, "Set up as the receiver.");
FLAG(sender, "Set up as the sender.");

enum Data {
  VAR_INT = 0,
  STRING = 1,
  DOUBLE = 2
};

struct VarIntMessage : public Message<VAR_INT> {
  VarIntMessage() : VarIntMessage(0) {}
  VarIntMessage(int64_t value) : payload(value) {}

  void encode(BinaryWriter& writer) const override {
    writer.writeVarInt(payload);
  }

  void decode(BinaryReader& reader) override { payload = reader.readVarInt(); }

  int64_t payload;
};

typedef PayloadMessage<STRING, string> StringMessage;
typedef PayloadMessage<DOUBLE, double> DoubleMessage;

void on_var_int(const VarIntMessage& message) {
  cout << "var_int " << message.payload << "\n";
}

void on_string(const StringMessage& message) {
  cout << "string \"" << message.payload << "\"\n";
}

void on_double(const DoubleMessage& message) {
  cout << "double " << message.payload << "\n";
}

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (!options::receiver && !options::sender) {
    cerr << "Please specify either --sender or --receiver.\n";
    return 1;
  }

  if (options::sender) {
    Socket socket;
    socket.connect(options::host, options::port);
    Messenger messenger(move(socket));

    // Repeatedly send messages.
    cout << ">> ";
    string type;
    cin >> type;

    while (cin) {
      if (type == "var_int") {
        uint64_t value;
        cin >> value;
        messenger.send(VarIntMessage(value));
      } else if (type == "string") {
        string line;
        getline(cin, line);
        messenger.send(StringMessage(line.substr(1)));
      } else if (type == "double") {
        double value;
        cin >> value;
        messenger.send(DoubleMessage(value));
      } else {
        cout << "Unrecognised type.\n";
      }

      cout << ">> ";
      cin >> type;
    }
  }

  if (options::receiver) {
    Socket server;
    server.bind(options::host, options::port);
    server.listen();
    Messenger messenger(move(server.accept()));

    messenger.on<VarIntMessage>(VAR_INT, on_var_int);
    messenger.on<StringMessage>(STRING, on_string);
    messenger.on<DoubleMessage>(DOUBLE, on_double);

    messenger.serve();
  }
}
