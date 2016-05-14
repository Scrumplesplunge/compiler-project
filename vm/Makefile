MAKEFLAGS = -j8
CXXFLAGS = -std=c++14 -pthread -Igen -I../lib/src -Ofast -s -flto -Wall  \
					 -Wl,--gc-sections

.PHONY: all clean

all: bin/as bin/das bin/vm bin/master bin/worker

# Sources.
SOURCES = $(shell find src -name '*.cc')
OBJECTS = $(patsubst src/%.cc, obj/%.o, ${SOURCES})
DEPENDS = $(patsubst src/%.cc, dep/%.d, ${SOURCES})
GEN = $(patsubst src/%.enum, gen/%.o, $(shell find src -name '*.enum'))

ARGS = $(addprefix ../lib/obj/util/, args.o table.o text.o atomic_output.o)
BINARY = $(addprefix ../lib/obj/util/, binary.o stream.o)
JSON = ../lib/obj/util/json.o
NETWORK = gen/Network.o obj/network.o ${BINARY}  \
					$(addprefix ../lib/obj/util/, messenger.o socket.o)
OPERATIONS = $(addprefix gen/, Direct.o Indirect.o Unit.o)
VM = $(addprefix obj/runtime/, VM.o VMDirect.o VMIndirect.o)

AS_DEPS =  \
  obj/assembler/operations.o ${ARGS} ${OPERATIONS}
DAS_DEPS =  \
  obj/util.o obj/assembler/operations.o ${ARGS} ${OPERATIONS}
VM_DEPS =  \
  obj/util.o obj/runtime/metadata.o  \
  ${ARGS} ${BINARY} ${JSON} ${OPERATIONS} ${VM}
MASTER_DEPS =  \
  obj/util.o obj/runtime/metadata.o  \
  $(addprefix obj/master/, config.o ProcessTree.o ProcessMaster.o)  \
  ${ARGS} ${JSON} ${BINARY} ${NETWORK} ${VM} ${OPERATIONS}
WORKER_DEPS =  \
  obj/util.o  \
  $(addprefix obj/worker/, Instance.o PartialProcessTree.o ProcessServer.o)  \
  ${ARGS} ${JSON} ${BINARY} ${NETWORK} ${OPERATIONS} ${VM}

bin/as: obj/assembler/as.o ${AS_DEPS} | bin
	${CXX} ${CXXFLAGS} $^ -o $@
bin/das: obj/assembler/das.o ${DAS_DEPS} | bin
	${CXX} ${CXXFLAGS} $^ -o $@
bin/vm: obj/runtime/vm.o ${VM_DEPS} | bin
	${CXX} ${CXXFLAGS} $^ -o $@
bin/master: obj/master/master.o ${MASTER_DEPS} | bin
	${CXX} ${CXXFLAGS} $^ -o $@
bin/worker: obj/worker/worker.o ${WORKER_DEPS} | bin
	${CXX} ${CXXFLAGS} $^ -o $@

# External dependencies.
ENUM = ../tools/bin/enum
${ENUM}:
	@echo "Building enum compiler.."
	${MAKE} -C ../tools bin/enum

../lib/%:
	${MAKE} -C ../lib $*

# Generated code.
gen/%.h gen/%.cc: src/%.enum | ${ENUM}
	@mkdir -p gen
	cd gen && ../${ENUM} ../$^ $*

gen/%.o: gen/%.cc | gen/%.h
	${CXX} ${CXXFLAGS} -c $^ -o $@

# Dependency graph.
dep/%.d: src/%.cc | ${GEN}
	@mkdir -p dep/$(dir $*)
	${CXX} ${CXXFLAGS} -MM -MF dep/$*.d $^ -MT obj/$*.o

# Object files.
obj/%.o: src/%.cc
	@mkdir -p obj/$(dir $*)
	${CXX} ${CXXFLAGS} -c $< -o $@

bin:
	mkdir bin

clean:
	rm -rf bin dep gen obj

-include ${DEPENDS}
