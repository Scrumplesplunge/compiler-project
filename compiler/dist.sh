#!/usr/bin/zsh

# Compile the source file.
echo "Compiling.."
bin/occ --assembly_file code.s --metadata_file metadata.json --source_file $1 
if [[ $? -ne 0 ]]; then
  echo "Compilation failed."
  exit 1
fi

# Assemble the code.
echo "Assembling.."
../vm/bin/as --assembly_file code.s --bytecode_file code.bin
if [[ $? -ne 0 ]]; then
  echo "Assembly failed."
  exit 1
fi

# Construct the job file.
echo "Constructing job file.."
>job.json <<EOF
{
  "name" : "$1",
  "description" : "Automatically generated job file.",
  "bytecode_file" : "code.bin",
  "metadata_file" : "metadata.json",
  "workers" : [
    {"host" : "localhost", "port" : 17994},
    {"host" : "localhost", "port" : 17995}
  ]
}
EOF

# Execute the binary.
echo "Executing program.."
../vm/bin/master --job_file job.json $2
if [[ $? -ne 0 ]]; then
  echo "VM terminated unsuccessfully.";
  exit 1
fi

# Delete the intermediates.
echo "Removing intermediate files.."
rm code.s metadata.json code.bin job.json
