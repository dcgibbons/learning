CFLAGS=-g -pedantic -Wall -std=c99 

all: gen_numbers exer03 test.dat
clean:
	-rm a.out *.o gen_numbers exer03 *.dat

gen_numbers: gen_numbers.c
exer03: exer03.c
test.dat: gen_numbers
	./gen_numbers > test.dat
