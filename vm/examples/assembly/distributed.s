INIT:
  ajw 64

START:
  ajw -4               # x, y, c, d
  ldlp 3
  resetch
  ldlp 4
  resetch

  # ...

  # Spawn the worker.
  ajw -1               # Allocate handle.
  ldc WORKER - STARTW  # Load code offset.
  ldpi                 # Make absolute.
  ldc WORKER_SIZE      # Load workspace size.
  startw               # Start the worker.
STARTW:
  stl 1

  # Transmit read variables.
  ldl 2                # Load the value of x.
  ldl 1                # Load the handle.
  sendvar

  # Transmit channels.
  ldlp 4               # Load the channel address for c.
  ldl 1                # Load the handle.
  sendchr              # Send c as a read channel.
  ldlp 5               # Load the channel address for d.
  ldl 1                # Load the handle.
  sendchw              # Send d as a write channel.

  # Wait for completion signal.
  ldl 1                # Load the handle.
  waitw                # Wait for the worker.

  # Receive the written variables.
  ldlp 3               # Load the address of y.
  ldl 1                # Load the handle.
  recvvar              # Receive the new value.

  # Receive the channels.
  ldlp 4               # Load the channel address for c.
  ldl 1                # Load the handle.
  recvchr              # Receive the reader for c.
  ldlp 5               # Load the channel address for d.
  ldl 1                # Load the handle.
  recvchw              # Receive the writer for d.

  # Shutdown.
  ldl 1                # Load the handle.
  closew               # Close the connection.
  ajw 1                # Deallocate the handle.

DONE:
  stopp                # Terminate the process.

WORKER:
  # Worker initializes with handle in workspace location 1.

  # Allocate local variables (a, b, x, y)

  # Receive the read variables.
  ldlp 1               # Load the address of x.
  ldl 5                # Load the handle.
  recvvar              # Receive the value.

  # Receive channels.
  ldlp 3               # Load the address of c.
  ldl 5                # Load the handle.
  recvchr              # Receive c as a reader.
  ldlp 4               # Load the address of d.
  ldl 5                # Load the handle.
  recvchw              # Receive d as a writer.

  # Run the process.
  P

  # Signal completion.
  ldl 5                # Load the handle.
  endw                 # Signal completion.

  # Transmit written variables.
  ldl 2                # Load the value of y.
  ldl 5                # Load the handle.
  sendvar

  # Return the borrowed channels.
  ldlp 4               # Load the channel address for c.
  ldl 1                # Load the handle.
  sendchr              # Send c as a read channel.
  ldlp 5               # Load the channel address for d.
  ldl 1                # Load the handle.
  sendchw              # Send d as a write channel.

  # Done.
  ldl 5                # Load the handle.
  closew               # Close the connection.
  stopp                # Terminate the process.
