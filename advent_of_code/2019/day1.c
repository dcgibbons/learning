/*
 * day1.c
 * Advent of Code - 2019 - Day 1
 * https://adventofcode.com/2019/day/1
 *
 * Chad Gibbons
 * December 4, 2019
 */

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

/* forward reference for internal functions */
static long calculate_fuel(const char* input_filename);

int main(int argc, char* argv[])
{
    int rc = EXIT_SUCCESS;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s [input data]\n", argv[0]);
        rc = EXIT_FAILURE;
    } else {
        const char* input_filename = argv[1];
        long total_fuel = calculate_fuel(input_filename);
        printf("Total fuel requirement: %lu\n", total_fuel);
    }

    return rc;
}

static long calculate_fuel(const char* input_filename)
{
    FILE* fp = fopen(input_filename, "r");
    if (!fp) {
        perror(input_filename);
        exit(errno);
    }

    long fuel = 0L;

    char buffer[BUFSIZ];
    while (fgets(buffer, sizeof(buffer), fp)) {
        char* next = NULL;
        long ret = strtol(buffer, &next, 10);
        if (ret == 0 && errno == EINVAL) {
            fprintf(stderr, "Invalid mass: %s\n", buffer);
            continue;
        }

        fuel += (ret / 3) - 2;
    }

    if (!feof(fp)) {
        perror(input_filename);
        exit(errno);
    }

    return fuel;
}

