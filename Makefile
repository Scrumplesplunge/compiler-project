CXXFLAGS = -std=c++11
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

all: bin/vm

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

bin/vm: src/vm.cc gen/Direct.cc gen/Indirect.cc  \
	    | bin gen/Direct.h gen/Indirect.h
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
