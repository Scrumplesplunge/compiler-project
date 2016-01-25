INIT:      ajw 64                 # Position the initial workspace.
           
           ldc 2                  # Load the number of processes to wait for.
           stl 1                  # Store in Wptr[1].
           ldc END - HERE         # Load the new process address.
           ldpi
HERE:      stl 0

           ajw -2                 # Allocate the channels.
           mint                   # Initialise channel 1.
           stl 1
           mint                   # Initialise channel 2.
           stl 2

START:     ldc SENDER - STARTP    # Load the offset to the sender process.
           ldlp -10               # Assign it a workspace.
           startp                 # Start it.
STARTP:    j BEGINALT - JUMP      # Begin the alternative.
JUMP:

# SENDER PROCESS
SENDER:    ldlp 12                # Channel 2.
           ldc 1234
           outword
           ldlp 12                # Exit and synchronise.
           endp

BEGINALT:  alt
           ldlp 1                 # Channel 1
           ldc 1                  # Always TRUE
           enbc

           ldlp 2                 # Channel 2
           ldc 1                  # Always TRUE
           enbc

           altwt                  # Start listening to the channels

           ldlp 1                 # Channel 1
           ldc 1                  # Always TRUE
           ldc SERVICE1 - ENDALT
           disc

           ldlp 2                 # Channel 2
           ldc 1                  # Always TRUE
           ldc SERVICE2 - ENDALT
           disc

           altend
ENDALT:
           ldc 58                 # This should not be executed.
           putc
           ldc 40
           putc
           stopp

SERVICE1:  ajw -8                 # Start of service.
           ldlp 1                 # Store in workspace
           ldlp 9                 # Channel 1
           ldc 4                  # Message length
           in
           ldc 49                 # '1: '
           putc               
           ldc 58
           putc
           ldc 32
           putc
           ldl 1                  # Display the result.
           printdec
           ajw 8                  # End of service.
           j CONTINUE - JUMP1
JUMP1:

SERVICE2:  ajw -8                 # Start of service.
           ldlp 1                 # Store in workspace
           ldlp 10                # Channel 2
           ldc 4                  # Message length
           in
           ldc 50                 # '2: '
           putc
           ldc 58
           putc
           ldc 32
           putc
           ldl 1
           printdec               # Display the result.
           ajw 8                  # End of service.
           j CONTINUE - JUMP2
JUMP2:

CONTINUE:  ldc 10                 # '\n'
           putc
           ajw 2                  # Deallocate the channels.
           ldlp 0                 # End.
           endp

END:
