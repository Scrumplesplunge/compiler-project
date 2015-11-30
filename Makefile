SOURCES = $(wildcard src/*.hs)

.PHONY: all clean run

all: bin/Parser

bin/Parser: ${SOURCES} | bin build
	ghc -O1 -hidir build -odir build -o $@ $^

build:
	mkdir build

bin:
	mkdir bin

clean:
	rm -rf bin build

run: bin/Parser
	bin/Parser <examples/code-mini.txt
