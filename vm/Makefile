CXXFLAGS = -std=c++11 -I gen -Ofast -s -flto -Wall -Wl,--gc-sections
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

all: bin/as bin/vm

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

bin/as: src/as.cc src/operations.cc gen/Direct.cc gen/Indirect.cc gen/Unit.cc  \
	    | bin src/operations.h gen/Direct.h gen/Indirect.h  \
			  gen/Unit.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/vm: src/vm.cc src/runtime/VM.cc src/operations.cc gen/Direct.cc  \
	      gen/Indirect.cc gen/Unit.cc src/runtime/VMDirect.cc  \
				src/runtime/VMIndirect.cc  \
			| bin src/runtime/VM.h src/operations.h gen/Direct.h gen/Indirect.h  \
			  gen/Unit.h
	${CXX} ${CXXFLAGS} $^ -o $@

gen/%.h: gen/%.cc
gen/%.cc: src/%.enum | ${ENUM} gen
	(cd gen; ../${ENUM} ../$< $*)

bin:
	mkdir bin

gen:
	mkdir gen

clean:
	rm -rf bin gen
