# Virtual Machine

This is an implementation of the transputer instruction set as a virtual
machine, written in pure C++, as well as an assembler for generating executable
bytecode. This should allow the virtual machine to run on any system for which
C++14 can be compiled. The instructions that are supported are listed in the
__*.enum__ files, and the implementation of the behaviour for each of these
instructions is in one of __VMDirect.cc__ or __VMIndirect.cc__, under the
__runtime__ subdirectory.

### Current Status

The virtual machine has all instructions from the transputer instruction set
implemented, with the following exceptions:

  * Delay scheduling (timer instructions).
  * External channels.
  * Floating point instructions.
  * Block-move instructions.

Of these, only the first two are likely to be implemented.
