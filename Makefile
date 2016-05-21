MAKEFLAGS = -j8

.PHONY: all clean compiler vm report tools status

all: compiler vm report tools

clean:
	${MAKE} -C compiler clean
	${MAKE} -C vm clean
	${MAKE} -C report clean
	${MAKE} -C tools clean

compiler:
	${MAKE} -C compiler

vm:
	${MAKE} -C vm

report:
	${MAKE} -C report

tools:
	${MAKE} -C tools

status:
	@echo "===== REPORT ====="
	@find report -name '*.tex' -not -name 'glossary*' |  \
	 xargs texcount -nosub -total |  \
	 tail -n +2
	@echo "===== CODE ====="
	@cloc -quiet . | tail -n +3
