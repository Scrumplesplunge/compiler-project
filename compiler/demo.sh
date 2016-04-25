#!/usr/bin/zsh

# Compile the source file.
bin/occ --source_file $1 --assembler_file code.s --data_file data.bin

# Assemble the code.
../vm/bin/as --source code.s --bytecode code.bin

# Execute the binary.
if ../vm/bin/vm --bytecode code.bin --data data.bin; then
  # Delete the intermediates.
  rm code.s code.bin data.bin
fi
