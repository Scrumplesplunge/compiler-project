#!/usr/bin/zsh

# Construct the assembler file.
cat >demo.s <<PRE
INIT:
  ajw 1024

START:
PRE

bin/occ $1 -o - >> demo.s

cat >> demo.s <<POST
END:
  stopp
POST

# Assemble the code.
../vm/bin/as --source demo.s --bytecode demo.bin

# Execute the binary.
if ../vm/bin/vm --bytecode demo.bin; then
  # Delete the intermediates.
  rm demo.s demo.bin
fi
