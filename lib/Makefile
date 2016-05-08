MAKEFLAGS = -j8
CXXFLAGS = -std=c++14 -Igen -I../lib -Ofast -s -flto -Wall -Wl,--gc-sections  \
					 -pthread

.PHONY: all clean

all: bin/socket_demo bin/messenger_demo bin/pipe_demo

ARGS = $(addprefix obj/util/, args table text)
BINARY = $(addprefix obj/util/, stream binary)

SOCKET_DEMO_LIBS = obj/util/socket ${ARGS} ${BINARY}
bin/socket_demo: $(addsuffix .o, obj/socket_demo ${SOCKET_DEMO_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

MESSENGER_DEMO_LIBS = $(addprefix obj/util/, messenger socket) ${ARGS} ${BINARY}
bin/messenger_demo:  \
		$(addsuffix .o, obj/messenger_demo ${MESSENGER_DEMO_LIBS}) | bin
	${CXX} ${CXXFLAGS} $^ -o $@

PIPE_DEMO_LIBS = obj/util/pipe ${BINARY}
bin/pipe_demo: $(addsuffix .o, obj/pipe_demo ${PIPE_DEMO_LIBS}) | bin 
	${CXX} ${CXXFLAGS} $^ -o $@

obj/%.o: src/%.cc | $(wildcard %.h) obj
	@mkdir -p $(dir $@)
	${CXX} ${CXXFLAGS} -c $^ -o $@

bin:
	mkdir bin

obj:
	mkdir obj

clean:
	rm -rf bin obj
