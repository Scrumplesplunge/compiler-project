INIT:       ajw 64                  # Allocate some workspace.

START:      ldc 2                   # Load the number of processes to wait for.
            stl 1                   # Store in Wptr[1].
            ldc END - HERE          # Load the END address.
            ldpi
HERE:       stl 0                   # Store in Wptr[0].

            ajw -2                  # Allocate the channel.
            mint                    # Initialise the channel with NotProcess.
            stl 1

            ldc PROC_2 - STARTP     # Load the offset to PROC_2.
            ldlp -20                # Allocate PROC_2 some workspace.
            startp                  # Start PROC_2.
STARTP:

PROC_1:     ajw -10                 # Allocate PROC_1 some workspace.
            ldc 1                   # Prepare to loop 10 times.
            stl 1
            ldc 10
            stl 2

LOOP_1:     ldc 62                  # Print '>'
            putc

            ldlp 11                 # Load channel address. 
            ldc 1                   # Send the ping.
            outword

            ldc 41                  # Print ')'
            putc

            ldlp 1                  # Loop.
            ldc ENDLOOP_1 - LOOP_1
            lend
ENDLOOP_1:

            ajw 10
            ldlp 2                  # End process.
            endp

PROC_2:     ldc 1                   # Prepare to loop 10 times.
            stl 1
            ldc 10
            stl 2

LOOP_2:     ldlp 1                  # Load destination address.
            ldlp 21                 # Load channel address.
            ldc 4                   # Load message size.
            in                      # Retrieve the message.

            ldc 125                 # Print '}'
            putc

            ldlp 1                  # Loop.
            ldc ENDLOOP_2 - LOOP_2
            lend
ENDLOOP_2:

            ldlp 22                 # End process.
            endp

END:        ldc 88                  # Print 'X'.
            putc
            ldc 10                  # Print '\n'.
            putc
