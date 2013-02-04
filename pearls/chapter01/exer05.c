/*
 * exer05.c
 * Exercise 1.5 from Programming Pearls
 * 
 * Same as exercise 1.3, but now we are limited to only 1MB of heap.
 *
 * This solution will make 2 passes through the file to produce the
 * sorted output file. This is still an O(n) algorithm with only a slightly 
 * larger (in milliseconds) total run-time than the exercise 1.3 solution.
 *
 * Chad Gibbons
 * September 7, 2012
 */

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VALUE       10000000
#define MAX_BITS        (int)(1024 * 1024 / sizeof(unsigned char) * CHAR_BIT)

/*
 * Store a bit in the bit vector, taking an offset into account. The offset 
 * allows us to store values larger than MAX_BITS in a smaller bit vector.
 */
static void store_bit(const int n, unsigned char* bits, int offset)
{
    assert (n > 0 && n < MAX_VALUE);

    int byte = (n / CHAR_BIT) + offset;
    int bit = n % CHAR_BIT;
    bits[byte] |= 1 << bit;
}

int main(void)
{
    // Allocate a bit vector no larger than MAX_BITS in size.
    const size_t bit_vector_size = MAX_BITS / CHAR_BIT * sizeof(unsigned char);
    unsigned char* bits = malloc(bit_vector_size);
    if (!bits)
    {
        fprintf(stderr, "unable to allocate %lu bytes: %s\n", 
                bit_vector_size, strerror(errno));
        exit(EXIT_FAILURE);
    }

    for (int pass = 0; pass < 2; pass++)
    {
        memset(bits, 0, MAX_BITS / CHAR_BIT);
        int offset = (pass == 0) ? 0 : -(MAX_BITS / CHAR_BIT);

        char buffer[BUFSIZ];
        while (fgets(buffer, sizeof(buffer), stdin) != NULL)
        {
            int n = atoi(buffer);
            if (pass == 0 && n < MAX_BITS)
                store_bit(n, bits, offset);
            else if (pass == 1 && n >= MAX_BITS)
                store_bit(n, bits, offset);
        }

        for (int i = (pass == 0) ? 0 : MAX_BITS, j = (pass == 0) ? MAX_BITS : MAX_VALUE; i < j; i++)
        {
            int byte = (i / CHAR_BIT) + offset;
            int bit = i % CHAR_BIT;
            if (bits[byte] & 1 << bit)
            {
                fprintf(stdout, "%07d\n", i);
            }
        }
    }

    free(bits);
    return 0;
}
