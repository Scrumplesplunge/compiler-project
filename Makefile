CXXFLAGS = -std=c++11 -I gen -Os -s -flto -Wl,--gc-sections -fdata-sections -ffunction-sections
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

all: bin/vm

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

bin/vm: src/vm.cc src/State.cc src/operations.cc gen/Direct.cc  \
	      gen/Indirect.cc gen/Unit.cc src/VMDirect.cc src/VMIndirect.cc \
	    | bin src/State.h src/operations.h gen/Direct.h gen/Indirect.h gen/Unit.h
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
