#!/usr/bin/zsh

# Construct the assembler file.
cat >demo.s <<PRE
INIT:
  ajw 1024

START:
PRE

bin/occ --source_file $1 --assembler_file demo.s --data_file - >> /dev/null

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
