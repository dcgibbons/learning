/*
 * bench.c
 * find gaps on a "bench" for newcomers to sit
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "priority_queue.h"

void find_spots(const char* bench, int nspots)
{
    for (int i = 0; i < 20; i++) {
        printf("%.1d", i % 10);
    }
    printf("\n%s\n", bench);

    const int n = strlen(bench);

    queue_t* pqueue = NULL;

    // skip the first edge
    int i = 0;
    while (i < n && bench[i] == ' ') {
        i++;
    }
    
    // look for gaps
    int current_gap = 0;
    int current_gap_pos = -1;
    for (; i < n; i++) {
        if (bench[i] == ' ') {
            current_gap++;
            current_gap_pos = (current_gap_pos < 0) ? i : current_gap_pos;
        } else {
            if (current_gap > 0) {
                // put the person in the middle of the current gap
                current_gap_pos += current_gap / 2;
                // invert priority to work with min priority queue
                queue_push(&pqueue, n - current_gap, (void*)(intptr_t)current_gap_pos);
            }
            current_gap = 0;
            current_gap_pos = -1;
        }
    }
    // don't push the last gap if we've reached the edge - skip the edge gaps!

    if (pqueue == NULL) {
        printf("there were no gaps on the bench!\n");
    } else {
        int spots = 0;
        while (spots < nspots && pqueue != NULL) {
            int gap_pos = (intptr_t)queue_pop(&pqueue);
            spots++;
            printf("person %d sit at pos %d\n", spots, gap_pos);
        }
        if (spots < nspots) {
            printf("there were not enough spots for everyone\n");
        }
    }

}

int main()
{
    find_spots("pppppp", 1);
    puts("");

    find_spots(" pp  p p p  p pp   p  ", 3);
    puts("");

    find_spots("    pp p  p  pppp p   ", 6);
    puts("");

    find_spots(" p p    pp   ppp      p      ", 4);
    puts("");
}
