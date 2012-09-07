CFLAGS=-g -pedantic -Wall -std=c99 

all: exer03 exer04 test.dat exer05
clean:
	-rm -r a.out *.o exer?? *.dat *.dSYM
exer03: exer03.c
exer04: exer04.c
test.dat: exer04
	./exer04 > test.dat
exer05: exer05.c
