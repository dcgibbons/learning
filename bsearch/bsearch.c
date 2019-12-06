/*
 * bsearch.c
 * simple binary search
 *
 * Chad Gibbons
 * December 5, 2019
 */

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

bool do_search(const int value, const int data[],
               const size_t min, const size_t max)
{
    // base case, not found!
    if (min > max) {
        return false;
    } else {
        int mid = (min + max) / 2;

        if (data[mid] == value) {
            return true;
        } else if (value < data[mid]) {
            return do_search(value, data, min, mid-1);
        } else {  // value > data[mid]
            return do_search(value, data, mid+1, max);
        }
    }
}

bool search(const int value, const int data[], const size_t n)
{
    return do_search(value, data, 0, n);
}

int main(int argc, char* argv[])
{
    int data[] = {1, 3, 4, 5, 9, 11, 17, 99, 157, 169, 503, 4831, 9999, 10093};
    const size_t n = sizeof(data)/sizeof(data[0]);

    for (int i = 0; i < n; i++) {
        assert(search(data[i], data, n));
    }

    puts("all values found successfully");

    return EXIT_SUCCESS;
}

