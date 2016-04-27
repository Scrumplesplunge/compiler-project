#include "util/args.h"
#include "util/binary.h"
#include "util/socket.h"

#include <iostream>
#include <string>

using namespace std;

OPTION(string, host, "0.0.0.0", "Host to connect to.");
OPTION(int, port, 12345, "Port to connect to.");

FLAG(receiver, "Set up as the receiver.");
FLAG(sender, "Set up as the sender.");

enum MessageType {
  VAR_INT,
  STRING,
  DOUBLE
};

int main(int argc, char* args[]) {
  args::process(&argc, &args);

  if (!options::receiver && !options::sender) {
    cerr << "Please specify either --sender or --receiver.\n";
    return 1;
  }

  if (options::sender) {
    Socket socket;
    socket.connect(options::host, options::port);
    BinaryWriter writer(socket);

    // Repeatedly send typed binary data.
    cout << ">> ";
    string type;
    cin >> type;

    while (cin) {
      if (type == "var_int") {
        writer.writeVarUint(VAR_INT);
        uint64_t value;
        cin >> value;
        writer.writeVarUint(value);
      } else if (type == "string") {
        writer.writeVarUint(STRING);
        string line;
        getline(cin, line);
        writer.writeString(line.substr(1));
      } else if (type == "double") {
        writer.writeVarUint(DOUBLE);
        double value;
        cin >> value;
        writer.writeDouble(value);
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
    Socket socket = server.accept();

    BinaryReader reader(socket);

    // Repeatedly receive typed binary data.
    while (true) {
      MessageType type;
      try {
        type = static_cast<MessageType>(reader.readVarUint());
      } catch (...) {
        // Stream has closed.
        return 0;
      }
      switch (type) {
        case VAR_INT:
          cout << "var_int " << reader.readVarUint() << "\n";
          break;
        case STRING:
          cout << "string \"" << reader.readString() << "\"\n";
          break;
        case DOUBLE:
          cout << "double " << reader.readDouble() << "\n";
          break;
        default:
          cout << "<INVALID>\n";
          return 1;
      }
    }
  }
}
