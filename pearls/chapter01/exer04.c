/*
 * exer04.c
 * Exercise 1.4 from Programming Pearls
 *
 * Generates a list of 7-digit numbers that are semantically valid North 
 * America Numbering Plan telephone numbers. Since we don't need an area code 
 * for the problem statement, we just need to make sure that the central office 
 * code begins with a digit between 2 and 9.
 *
 * Chad Gibbons
 * September 6, 2012
 */

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_VALUE       10000000
#define MAX_NUMBERS     1000000

int main(void)
{
    /* use a bit vector here to ensure we don't generate the same telephone 
     * number more than once.
     */
    unsigned char* bits = calloc(MAX_VALUE / CHAR_BIT, sizeof(unsigned char));
    if (!bits)
    {
        fprintf(stderr, "Unable to allocate %lu bytes: %s\n",
                MAX_VALUE / CHAR_BIT * sizeof(unsigned char),
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    (void)srand(time(NULL));

    for (int i = 0; i < MAX_NUMBERS; i++)
    {
        while (true)
        {
            const int n = (rand() % MAX_NUMBERS) + 2000000;
            const int byte = n / CHAR_BIT;
            const int bit = n % CHAR_BIT;
            if (bits[byte] & 1 << bit)
            {
                // generate another number if we've already done this one
                continue;
            }
            else
            {
                bits[byte] |= 1 << bit;
                fprintf(stdout, "%07d\n", n);
                break;
            }
        }
    }

    return 0;
}

