
CFLAGS=-Wall -g -std=c99

binaries=bench linked_list maze_dijkstra permutations permute_words \
	 predictive_text rain_on_landscape rearrange_array

all: clean $(binaries)

bench: bench.c priority_queue.c

linked_list: linked_list.c

maze_dijkstra: maze_dijkstra.c priority_queue.c hash_table.c

permutations: permutations.c

permute_words: permute_words.c

predictive_text: predictive_text.c

predictive_text_compact: predictive_text_compact.c

rearrange_array: rearrange_array.c

rain_on_landscape: rain_on_landscape.c

clean:
	-rm *.o
	-rm *~
	-rm $(binaries)
