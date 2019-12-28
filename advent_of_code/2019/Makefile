
DEFINES=-D_POSIX_C_SOURCE=200809L
CFLAGS=-Wall -g -std=c99 $(DEFINES) -lm
CUNIT_LDFLAGS=-lcunit -lcurses

binaries=day1 day1_part2 day2 day2_part2 day5 day5_part2 day7 day7_part2 \
	 day9 day9_part2 day11 day11_part2 day13 day13_part2 day15 \
	 day15_part2

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

day11: day11.c intcode.c

day11_part2: day11_part2.c intcode.c

day13: day13.c intcode.c 
	$(CC) $(CFLAGS) -o $@ $^ -lncurses

day13_part2: day13_part2.c intcode.c 
	$(CC) $(CFLAGS) -o $@ $^ -lncurses

day15: day15.c intcode.c priority_queue.c hash_table.c
	$(CC) $(CFLAGS) -o $@ $^

day15_part2: day15_part2.c intcode.c priority_queue.c hash_table.c
	$(CC) $(CFLAGS) -o $@ $^

