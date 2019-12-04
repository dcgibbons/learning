/*
 * rearrange_array.c
 * rearrange array from one arrangement to another
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void rearrange(const int n, const int initial[], const int destination[])
{
    int positions[n];
    int work[n];
    for (int i = 0; i < n; i++) {
        positions[initial[i]] = i;
        work[i] = initial[i];
    }

    for (int i = 0; i < n; i++) {
        // skip the slot if it already matches the destination
        if (work[i] == destination[i]) {
            continue;
        }

        // first, move this position to the empty slot
        int pos0 = positions[0];
        work[pos0] = work[i], positions[work[i]] = pos0;
        positions[0] = i;
        work[i] = 0;

        // second, move the item that wants this slot into it
        if (work[i] != destination[i]) {
            int wanted = destination[i];
            int posn = positions[wanted];

            positions[wanted] = i, work[i] = wanted;        // swap wanted with current (0)
            positions[0] = posn, work[posn] = 0;            // move 0 to old wanted pos
        }
    }

    for (int i = 0; i < n; i++) {
        printf("%d", work[i]);
        if (i < (n-1)) printf(",");
    }
    puts("");
}

int main()
{
    int source[] = {2,1,0,3};
    int dest[] = {3,0,1,2};
    rearrange(4, source, dest);

    int s2[] = {9,8,1,0,7};
    int d2[] = {1,7,0,8,9};
    rearrange(5, s2, d2);
    return EXIT_SUCCESS;
}

