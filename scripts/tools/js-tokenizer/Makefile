.PHONY: all clean compile

all: build compile

build:
	-mkdir build
	cd build && cmake ..

clean:
	rm -fr build

compile: build
	cd build &&	make
