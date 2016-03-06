SOURCES = $(wildcard src/*.hs)

.PHONY: all clean run

all: bin/occ

bin/occ: ${SOURCES} | bin build
	ghc -O -hidir build -odir build -o $@ $^

build:
	mkdir build

bin:
	mkdir bin

clean:
	rm -rf bin build

run: bin/occ
	bin/occ <examples/code-mini.txt
