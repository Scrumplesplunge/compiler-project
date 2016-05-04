CXXFLAGS = -std=c++11 -Igen -I../lib -Ofast -s -flto -Wall -Wl,--gc-sections
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

all: bin/as bin/das bin/vm bin/master bin/worker

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

bin/as: src/assembler/as.cc src/assembler/operations.cc  \
	      gen/Direct.cc gen/Indirect.cc gen/Unit.cc  \
	      ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
	    | bin src/assembler/operations.h  \
			  gen/Direct.h gen/Indirect.h gen/Unit.h  \
			  ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/das: src/assembler/das.cc src/assembler/operations.cc  \
				 src/util.cc  \
	       gen/Direct.cc gen/Indirect.cc gen/Unit.cc  \
				 ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
	     | bin src/assembler/operations.h  \
			   gen/Direct.h gen/Indirect.h gen/Unit.h  \
			   ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h  \
				 src/util.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/vm: src/runtime/vm.cc src/runtime/VM.cc src/runtime/VMDirect.cc  \
	      src/runtime/VMIndirect.cc  \
				gen/Direct.cc gen/Indirect.cc gen/Unit.cc  \
			  ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
				../lib/util/binary.cc ../lib/util/stream.cc  \
				src/util.cc  \
			| bin src/runtime/VM.h  \
			  gen/Direct.h gen/Indirect.h gen/Unit.h  \
			  ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h  \
				../lib/util/binary.h ../lib/util/stream.h  \
				src/util.h
	${CXX} ${CXXFLAGS} -DDISABLE_BOUND_CHECKS $^ -o $@

bin/master: src/master/master.cc src/master/config.cc  \
	          ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
						../lib/util/json.cc ../lib/util/socket.cc ../lib/util/stream.cc  \
						../lib/util/messenger.cc  \
						src/util.cc  \
					| bin src/master/config.h  \
					  ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h  \
						../lib/util/json.h ../lib/util/socket.h ../lib/util/stream.h  \
						../lib/util/messenger.h  \
						src/util.h 
	${CXX} ${CXXFLAGS} $^ -o $@

bin/worker: src/worker/worker.cc  \
	          ../lib/util/args.cc ../lib/util/table.cc ../lib/util/text.cc  \
						../lib/util/json.cc ../lib/util/socket.cc ../lib/util/stream.cc  \
						../lib/util/messenger.cc  \
						src/util.cc  \
					| bin src/master/config.h  \
					  ../lib/util/args.h ../lib/util/table.h ../lib/util/text.h  \
						../lib/util/json.h ../lib/util/socket.h ../lib/util/stream.h  \
						../lib/util/messenger.h  \
						src/util.h 
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
