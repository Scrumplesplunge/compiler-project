.PHONY: all clean

all:
	ghc Lexer.hs

clean:
	rm -rf *.hi *.o Lexer
