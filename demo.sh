#!/usr/bin/zsh

cat >demo.s <<PRE
INIT:
  ajw 128

START:
PRE

bin/Parser <$1 >> demo.s

cat >> demo.s <<POST
END:
  ldlp -9   # Load the address of x.
  ldc 10    # Load the size of x.
  printr    # Display the contents of x.
POST
../vm/bin/vm <demo.s
rm demo.s
