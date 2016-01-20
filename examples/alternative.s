        alt
        ldc 0x80000000  # Channel 1
        ldc 1           # Always TRUE
        enbc

        ldc 0x80000004  # Channel 2
        ldc 1           # Always TRUE
        enbc

        altwt           # Start listening to the channels

        ldc 0x80000000  # Channel 1
        ldc 1           # Always TRUE
        ldc SERVICE1 - ENDALT
        disc

        ldc 0x80000004  # Channel 2
        ldc 1           # Always TRUE
        ldc SERVICE2 - ENDALT
        disc

        altend
ENDALT:

SERVICE1:
        ajw -8
        ldlp 0          # Store in workspace
        ldc 0x80000000  # Channel 1
        ldc 128         # Message length
        in
JUMP1:  j CONTINUE - JUMP1

SERVICE2:
        ajw -8
        ldlp 0          # Store in workspace
        ldc 0x80000004  # Channel 2
        ldc 128         # Message length
        in
JUMP2:  j CONTINUE - JUMP2

CONTINUE:
