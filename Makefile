.PHONY: all clean

all:
	ghc Parser.hs

clean:
	rm -rf *.hi *.o Parser
