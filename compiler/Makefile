SOURCES = $(wildcard src/*.hs)

.PHONY: all clean run

all: bin/occ

bin/occ: ${SOURCES} | bin build
	ghc --make -j8 -split-objs -dynamic -i. -O2 -hidir build -odir build -o $@ $^
	# Reduce the binary size.
	strip -p --strip-unneeded --remove-section=.comment $@
	upx $@

build:
	mkdir build

bin:
	mkdir bin

clean:
	rm -rf bin build

run: bin/occ
	bin/occ <examples/code-mini.txt
