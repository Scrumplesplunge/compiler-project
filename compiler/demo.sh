#!/usr/bin/zsh

# Compile the source file.
bin/occ --source_file $1 --assembly_file code.s --data_file data.bin
if [[ $? -ne 0 ]]; then
  echo "Compilation failed."
  exit 1
fi

# Assemble the code.
../vm/bin/as --assembly_file code.s --bytecode_file code.bin
if [[ $? -ne 0 ]]; then
  echo "Assembly failed."
  exit 1
fi

# Execute the binary.
../vm/bin/vm --bytecode_file code.bin --data_file data.bin $2
if [[ $? -ne 0 ]]; then
  echo "VM terminated unsuccessfully.";
  exit 1
fi

# Delete the intermediates.
rm code.s code.bin data.bin
