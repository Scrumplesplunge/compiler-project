#!/usr/bin/zsh

# Compile the source file.
bin/occ --source_file $1 --assembly_file code.s --data_file data.bin

# Assemble the code.
../vm/bin/as --assembly_file code.s --bytecode_file code.bin

# Execute the binary.
if ../vm/bin/vm --bytecode_file code.bin --data_file data.bin; then
  # Delete the intermediates.
  rm code.s code.bin data.bin
fi
