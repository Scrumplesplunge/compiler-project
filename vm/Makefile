MAKEFLAGS = -j16
CXXFLAGS = -std=c++14 -Igen -I../lib -Ofast -s -flto -Wall -Wl,--gc-sections
TOOLS = ../tools
ENUM = ${TOOLS}/bin/enum

# Libraries.
ARGS = $(addprefix ../lib/util/, args table text)
BINARY = $(addprefix ../lib/util/, binary stream)
JSON = ../lib/util/json
NETWORK = src/network gen/Network  \
					$(addprefix ../lib/util/, binary messenger stream socket)
OPERATIONS = $(addprefix gen/, Direct Indirect Unit)

.PHONY: all clean

all: bin/as bin/das bin/vm bin/master bin/worker

${ENUM}:
	@echo "Building enum compiler.."
	make -C ../tools bin/enum

BIN_AS_LIBS = src/assembler/operations ${ARGS} ${OPERATIONS}
bin/as: $(addsuffix .cc, src/assembler/as ${BIN_AS_LIBS})  \
	    | bin $(addsuffix .h, ${BIN_AS_LIBS})
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_DAS_LIBS = src/util src/assembler/operations ${ARGS} ${OPERATIONS}
bin/das: $(addsuffix .cc, src/assembler/das ${BIN_DAS_LIBS})  \
	     | bin $(addsuffix .h, ${BIN_DAS_LIBS})
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_VM_LIBS = src/util src/runtime/VM ${ARGS} ${BINARY} ${OPERATIONS}
BIN_VM_SRC = $(addprefix src/runtime/, vm VMDirect VMIndirect)
bin/vm: $(addsuffix .cc, ${BIN_VM_SRC} ${BIN_VM_LIBS})  \
			| bin $(addsuffix .h, ${BIN_VM_LIBS})
	${CXX} ${CXXFLAGS} -DDISABLE_BOUND_CHECKS $^ -o $@

BIN_MASTER_LIBS = src/util $(addprefix src/master/, config ProcessMaster) \
									${ARGS} ${JSON} ${NETWORK}
bin/master: $(addsuffix .cc, src/master/master ${BIN_MASTER_LIBS})  \
					| bin $(addsuffix .h, ${BIN_MASTER_LIBS})
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_WORKER_LIBS = src/util src/worker/ProcessServer ${ARGS} ${JSON} ${NETWORK}
bin/worker: $(addsuffix .cc, src/worker/worker ${BIN_WORKER_LIBS})  \
					| bin $(addsuffix .h, ${BIN_WORKER_LIBS})
	${CXX} ${CXXFLAGS} $^ -o $@

gen/%.h gen/%.cc: src/%.enum | ${ENUM} gen
	(cd gen; ../${ENUM} ../$< $*)

obj/%.o:

bin:
	mkdir bin

gen:
	mkdir gen

clean:
	rm -rf bin gen obj
