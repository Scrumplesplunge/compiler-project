.PHONY: all build clean

all: build/report.pdf

build:
	mkdir -p build

clean:
	rm -rf build

build/report.pdf: build $(shell find -name '*.tex')
	# The document is built twice to ensure references are built correctly.
	pdflatex -output-directory=build report.tex
	pdflatex -output-directory=build report.tex
