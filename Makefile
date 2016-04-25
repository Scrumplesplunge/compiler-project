.PHONY: all clean compiler vm report tools

all: compiler vm report tools

clean:
	make -C compiler clean
	make -C vm clean
	make -C report clean
	make -C tools clean

compiler:
	make -C compiler

vm:
	make -C vm

report:
	make -C report

tools:
	make -C tools
