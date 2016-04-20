CXXFLAGS = -std=c++11 -Igen -I../lib -Ofast -s -flto -Wall -Wl,--gc-sections
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

all: bin/as bin/das bin/vm

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

bin/as: src/as.cc src/operations.cc gen/Direct.cc gen/Indirect.cc gen/Unit.cc  \
	      ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
	    | bin src/operations.h gen/Direct.h gen/Indirect.h gen/Unit.h  \
			  ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/das: src/das.cc src/operations.cc gen/Direct.cc gen/Indirect.cc  \
	       gen/Unit.cc ../lib/util/args.cc ../lib/util/table.cc  \
				 ../lib/util/text.cc  \
	     | bin src/operations.h gen/Direct.h gen/Indirect.h gen/Unit.h  \
			   ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/vm: src/vm.cc src/runtime/VM.cc src/operations.cc gen/Direct.cc  \
	      gen/Indirect.cc gen/Unit.cc src/runtime/VMDirect.cc  \
				src/runtime/VMIndirect.cc ../lib/util/args.cc ../lib/util/table.cc  \
				../lib/util/text.cc  \
			| bin src/runtime/VM.h src/operations.h gen/Direct.h gen/Indirect.h  \
			  gen/Unit.h ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h
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
