MAKEFLAGS = -j8
CXXFLAGS = -std=c++14 -Igen -I../lib/src -Ofast -s -flto -Wall -Wl,--gc-sections
ENUM = ../tools/bin/enum

# Generated code.
GEN = $(addprefix gen/, Direct Indirect Unit Network)

# Libraries.
ARGS = $(addprefix ../lib/obj/util/, args table text)
BINARY = $(addprefix ../lib/obj/util/, binary stream)
JSON = ../lib/obj/util/json
NETWORK = gen/Network obj/network  \
					$(addprefix ../lib/obj/util/, binary messenger stream socket)
OPERATIONS = $(addprefix gen/, Direct Indirect Unit)

.PHONY: all clean

all: bin/as bin/das bin/vm bin/master bin/worker

${ENUM}:
	@echo "Building enum compiler.."
	${MAKE} -C ../tools bin/enum

BIN_AS_LIBS = ${ARGS} ${OPERATIONS} obj/assembler/operations 
bin/as: $(addsuffix .o, obj/assembler/as ${BIN_AS_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_DAS_LIBS = ${ARGS} ${OPERATIONS} obj/util obj/assembler/operations
bin/das: $(addsuffix .o, obj/assembler/das ${BIN_DAS_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_VM_LIBS = ${ARGS} ${BINARY} ${OPERATIONS} obj/util obj/runtime/VM
BIN_VM = $(addprefix obj/runtime/, vm VMDirect VMIndirect)
bin/vm: $(addsuffix .o, ${BIN_VM} ${BIN_VM_LIBS}) | bin
	${CXX} ${CXXFLAGS} -DDISABLE_BOUND_CHECKS $^ -o $@

BIN_MASTER_LIBS = obj/util $(addprefix obj/master/, config ProcessMaster) \
									${ARGS} ${JSON} ${NETWORK}
bin/master: $(addsuffix .o, obj/master/master ${BIN_MASTER_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

BIN_WORKER_LIBS = ${ARGS} ${JSON} ${NETWORK} obj/util obj/worker/ProcessServer
bin/worker: $(addsuffix .o, obj/worker/worker ${BIN_WORKER_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

gen/%.h gen/%.cc: src/%.enum | ${ENUM} gen
	cd gen && ../${ENUM} ../$^ $*

gen/%.o: gen/%.cc | gen/%.h
	${CXX} ${CXXFLAGS} -c $^ -o $@

../lib/%:
	${MAKE} -C ../lib $*

obj/%.o: src/%.cc | $(wildcard %.h) obj $(addsuffix .o, ${GEN})
	@mkdir -p $(dir $@)
	${CXX} ${CXXFLAGS} -c $^ -o $@

bin:
	mkdir bin

gen:
	mkdir gen

obj:
	mkdir obj

clean:
	rm -rf bin gen obj
