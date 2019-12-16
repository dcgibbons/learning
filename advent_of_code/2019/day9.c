/*
 * day9.c
 * Advent of Code - 2019 - Day 9
 * https://adventofcode.com/2019/day/9
 *
 * Chad Gibbons
 * December 13, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "intcode.h"

/* forward reference */

int main(int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [intcode program]\n", argv[0]);
        exit(EXIT_FAILURE);
    } else {
        const char* input_filename = argv[1];
        intcode_t intcode = read_intcode(input_filename);
        if (intcode == NULL) {
            perror("unable to read intcode");
            exit(errno);
        }
        long input = 1;
        long output = -1;
        bool halted = false;
        while (!halted) {
            halted = run_intcode(intcode, &input, 1, &output);
            printf("output:%ld\n", output);
        }
    }
    return EXIT_SUCCESS;
}

