INIT:      ajw 64                 # Position the initial workspace.
           
START:     ldc 3                  # Load the number of processes to wait for.
           stl 1                  # Store in Wptr[1].
           ldc END - HERE         # Load END address.
           ldpi

HERE:      stl 0                  # Store in Wptr[0].

           ldc PROC_1 - STARTP_1  # Load offset to PROC_1.
           ldlp -10               # Assign a workspace.
           startp                 # Start PROC_1.

STARTP_1:  ldc PROC_2 - STARTP_2  # Load offset to PROC_2.
           ldlp -20               # Assign a workspace.
           startp                 # Start PROC_2.

STARTP_2:  ldlp 0                 # Wait for PROC_1 and PROC_2, then run END.
           endp

PROC_1:    ldc 1                  # Prepare to loop 10 times.
           stl 1
           ldc 10
           stl 2

LOOP_1:    ldc 49
           putc                   # Print '1'.
           ldlp 1
           ldc ENDLOOP_1 - LOOP_1
           lend

ENDLOOP_1: ldlp 10
           endp

PROC_2:    ldc 1                  # Prepare to loop 10 times.
           stl 1
           ldc 10
           stl 2

LOOP_2:    ldc 50
           putc                   # Print '2'.
           ldlp 1
           ldc ENDLOOP_2 - LOOP_2
           lend

ENDLOOP_2: ldlp 20
           endp

END:       ldc 88
           putc                   # Print 'X'.
           ldc 10
           putc                   # Print '\n'.
           stopp
