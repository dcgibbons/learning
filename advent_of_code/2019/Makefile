
CFLAGS=-Wall -g -std=c99

binaries=day1 day1_part2 day2 day2_part2 day5 day5_part2 day7 day7_part2 \
	 day9 day9_part2

all: clean $(binaries)

clean:
	-rm -f *~ 
	-rm -f *.o *.beam
	-rm -fr *.dSYM
	-rm -f $(binaries)

day1: day1.c

day1_part2: day1_part2.c

day2: day2.c

day2_part2: day2_part2.c

day5: day5.c

day5_part2: day5_part2.c

day7: day7.c intcode.c

day7_part2: day7_part2.c intcode.c

day9: day9.c intcode.c

day9_part2: day9_part2.c intcode.c
