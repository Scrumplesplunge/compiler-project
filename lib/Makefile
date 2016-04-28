CXXFLAGS = -std=c++14 -Igen -I../lib -Ofast -s -flto -Wall -Wl,--gc-sections  \
					 -pthread

all: bin/socket_demo bin/messenger_demo bin/pipe_demo

bin/socket_demo: socket_demo.cc util/socket.cc util/args.cc util/table.cc  \
	               util/text.cc util/stream.cc util/binary.cc  \
							 | bin util/socket.h util/args.h util/table.h util/text.h  \
							   util/stream.h util/binary.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/messenger_demo: messenger_demo.cc util/messenger.cc util/socket.cc  \
	                  util/args.cc util/table.cc util/text.cc util/stream.cc  \
										util/binary.cc  \
							    | bin util/socket.h util/args.h util/table.h util/text.h  \
							      util/stream.h util/binary.h util/messenger.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin/pipe_demo: pipe_demo.cc util/stream.cc util/pipe.cc  \
						 | bin util/stream.h util/pipe.h
	${CXX} ${CXXFLAGS} $^ -o $@

bin:
	mkdir bin

clean:
	rm -rf bin
