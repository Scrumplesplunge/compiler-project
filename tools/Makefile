CXXFLAGS = -std=c++11

all: bin/enum bin/enum_test

bin/enum: src/enum.cc | bin
	${CXX} ${CXXFLAGS} $^ -o $@

gen/Foo.cc: bin/enum | gen
	(cd gen; ../bin/enum ../src/Foo.enum Foo)

bin/enum_test: src/enum_test.cc gen/Foo.cc | gen
	${CXX} -Igen ${CXXFLAGS} $^ -o $@

bin:
	mkdir bin

gen:
	mkdir gen

clean:
	rm -rf bin gen
