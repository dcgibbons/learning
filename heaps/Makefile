#
# Makefile
# Heap Data Structure written in C
#
# David C. Gibbons
# dcgibbons@gmail.com
# November 23, 2019
# 

CFLAGS=-Wall -g -std=c99 -fprofile-arcs -ftest-coverage
LDFLAGS=-fprofile-arcs -ftest-coverage

all: test_heap

test_heap: heap.o test_heap.o

heap.o: heap.c heap.h

test_heap.o: test_heap.c heap.h

clean:
	-rm -fr *~
	-rm -fr *.o
	-rm -fr *.gcov *.gc??
	-rm test_heap

