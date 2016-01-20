INIT:    ajw 64              # Allocate some workspace.

START:   ldc 0x6c6c6548      # "Hell"
         stl 1
         ldc 0x57202c6f      # "o, W"
         stl 2
         ldc 0x646c726f      # "orld"
         stl 3
         ldc 0x0a21          # "!\n"
         stl 4
         # String is now stored in locals 1-4.
         ldlp 1              # Load the string pointer.
         stl 5
         ldc 14              # Load the string length.
         stl 6
         # Loop index state now in locals 5-6.

LOOP:    ldl 5               # Load the address of the next character.
         lb                  # Load the character.
         putc                # Print it (this is a debugging feature).
         ldlp 5
         ldc ENDLOOP - LOOP  # Load the loopback.
         lend                # End the loop.
ENDLOOP:
