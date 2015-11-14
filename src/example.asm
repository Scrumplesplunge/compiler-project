# This is an example extracted from the transputer booklet. However, I am not
# convinced that it works. The manual lists "j" as a descheduling point, so that
# means that each of the processes spawned by startp have a chance to start
# before the parent process reaches HERE. Therefore, I'm not sure how it can
# guarantee that the number of processes to synchronise will have been set
# before any of them tries to decrement the reference count..

  ldc PROC_X - STARTP1    # load distance between startp and PROC_X
  ldlp -6                 # load address of PROC_X Wptr = Wptr[-6]
STARTP1:
  startp                  # start the process located at PROC_X
  j CONT1                 # continue the current thread of execution at CONT1
PROC_X:
  ldc 2                   # load 2 into A
  stl x                   # x = 2
  ldlp -5                 # load address of new process Iptr
  endp                    # end this process, synchronize processes via endp
CONT1:
  ldc PROC_Y - STARTP2    # load distance between startp and PROC_Y
  ldlp -12                # load address of PROC_Y Wptr = Wptr[-12]
STARTP2:
  startp                  # start the process located at PROC_Y
  j CONT2                 # continue the current thread of execution at CONT2
PROC_Y:
  ldc 3                   # load 3 into A
  stl y                   # y = 3
  ldlp -11                # load address of new process Iptr
  endp                    # end this process, synchronize processes via endp
CONT2:
  ldc END - HERE          # load offset to END
  ldpi                    # load address of HERE + offset to END which equals
                          # the address of END
HERE:
  stl -1                  # store address into Wptr[-1] (will be future Iptr)
  ldc 3                   # load the number of processes to synchronize (three
                          # processes in this case)
  stl 0                   # save into Wptr[0]
  ldlp -1                 # load address of new process Iptr
  endp                    # end this process, start new process when all three
                          # processes end.
END:
