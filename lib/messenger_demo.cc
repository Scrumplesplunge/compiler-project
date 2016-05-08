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

struct VarInt {
  int64_t value;
};

template <>
VarInt BinaryReader::read<VarInt>() {
  return VarInt{readVarInt()};
}

template <>
void BinaryWriter::write<VarInt>(VarInt var_int) {
  writeVarInt(var_int.value);
}

void on_var_int(VarInt&& message) {
  cout << "var_int " << message.value << "\n";
}

void on_string(string&& message) {
  cout << "string \"" << message << "\"\n";
}

void on_double(double&& message) {
  cout << "double " << message << "\n";
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
        int64_t value;
        cin >> value;
        messenger.send(VAR_INT, VarInt{value});
      } else if (type == "string") {
        string line;
        getline(cin, line);
        messenger.send(STRING, line.substr(1));
      } else if (type == "double") {
        double value;
        cin >> value;
        messenger.send(DOUBLE, value);
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

    messenger.on<VarInt>(VAR_INT, on_var_int);
    messenger.on<string>(STRING, on_string);
    messenger.on<double>(DOUBLE, on_double);

    messenger.serve();
  }
}
