.PHONY: all clean compiler vm report tools status

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

status:
	@echo "===== REPORT ====="
	@find report -name '*.tex' | xargs texcount -nosub -total | tail -n +2
	@echo "===== CODE ====="
	@cloc -quiet . | tail -n +3
