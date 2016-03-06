.PHONY: all clean compiler report tools

all: compiler report tools

clean:
	make -C compiler clean
	make -C report clean
	make -C tools clean

compiler:
	make -C compiler

report:
	make -C report

tools:
	make -C tools
