#!/usr/bin/zsh

# Construct the assembler file.
cat >code.s <<PRE
INIT:
  ajw 1024

START:
PRE

# The assembler file is appended to rather than overwritten.
bin/occ --source_file $1 --assembler_file - --data_file data.bin >> code.s

cat >> code.s <<POST
END:
  stopp
POST

# Assemble the code.
../vm/bin/as --source code.s --bytecode code.bin

# Execute the binary.
if ../vm/bin/vm --bytecode code.bin --data data.bin; then
  # Delete the intermediates.
  rm code.s code.bin data.bin
fi
