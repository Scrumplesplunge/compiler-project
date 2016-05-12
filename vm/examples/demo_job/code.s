  ajw -1               # Allocate space for the instance handle.

  ldlp 1               # Load the address of the instance handle.
  ldc 2                # Load the number of words required by it.
  ldc REMOTE - HERE_1  # Load the address of the remote routine.
  ldpi
HERE_1:
  starti

  ldc 1                # Do something before waiting.
  printdec

  ldl 1                # Wait for the instance to terminate.
  joini

  ldc 3                # Do something after waiting.
  printdec

  stopp

REMOTE:
  ldc 2                # Do something remotely.
  printdec

  stopp
