.PHONY: all build clean

all: build/report.pdf

build:
	mkdir -p build

clean:
	rm -rf build

build/report.pdf: build $(shell find -name '*.tex')
	pdflatex -output-directory=build report.tex
	makeglossaries -d build report
