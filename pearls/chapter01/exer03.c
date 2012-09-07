/*
 * exer03.c
 * Exercise 1.3 from Programming Pearls
 * 
 * Sort a disk file of 7-digit numbers that are valid telephone numbers.  
 * Assumptions include that a phone number can only appear in the file once.  
 * This implementation uses a bit vector to indicate which numbers are present, 
 * and then dumps the result to the output file, resulting in a very fast 
 * domain-specific sort.
 *
 * Chad Gibbons
 * September 6, 2012
 */

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VALUE 10000000

int main(int argc, char *argv[])
{
    // allocate a bit vector large enough to hold MAX_VALUE. We can't assume 
    // this platform has 8-bits in an unsigned char, so we use the CHAR_BIT 
    // limit to correctly determine how many bits are in each char type.
    unsigned char* bits = calloc(MAX_VALUE / CHAR_BIT, sizeof(unsigned char));
    if (!bits)
    {
        fprintf(stderr, "unable to allocate %lu bytes: %s\n", 
                MAX_VALUE / CHAR_BIT * sizeof(unsigned char), 
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    char buffer[BUFSIZ];
    while (fgets(buffer, sizeof(buffer), stdin) != NULL)
    {
        int n = atoi(buffer);
        assert (n > 0 && n < MAX_VALUE);

        int byte = n / CHAR_BIT;
        int bit = n % CHAR_BIT;
        bits[byte] |= 1 << bit;
    }

    for (int i = 0; i < MAX_VALUE; i++)
    {
        int byte = i / CHAR_BIT;
        int bit = i % CHAR_BIT;
        if (bits[byte] & 1 << bit)
        {
            fprintf(stdout, "%07d\n", i);
        }
    }

    free(bits);
    return 0;
}
