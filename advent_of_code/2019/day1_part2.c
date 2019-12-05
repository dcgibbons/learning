/*
 * day2.c
 * Advent of Code - 2019 - Day 2
 * https://adventofcode.com/2019/day/2
 *
 * Chad Gibbons
 * December 4, 2019
 */

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

/* forward reference for internal functions */
static long calculate_total_fuel(const char* input_filename);

int main(int argc, char* argv[])
{
    int rc = EXIT_SUCCESS;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s [input data]\n", argv[0]);
        rc = EXIT_FAILURE;
    } else {
        const char* input_filename = argv[1];
        long total_mass = calculate_total_fuel(input_filename);
        printf("Total fuel requirement: %lu\n", total_mass);
    }

    return rc;
}

static long calculate_fuel(long mass)
{
    long fuel_required = (mass / 3) - 2;
    if (fuel_required <= 0) {
        return 0;
    } else {
        return fuel_required + calculate_fuel(fuel_required);
    }
}

static long calculate_total_fuel(const char* input_filename)
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
        long module_mass = strtol(buffer, &next, 10);
        if (module_mass == 0 && errno == EINVAL) {
            fprintf(stderr, "Invalid mass: %s\n", buffer);
            continue;
        }

        long module_fuel_required = calculate_fuel(module_mass);
        fuel += module_fuel_required;
    }

    if (!feof(fp)) {
        perror(input_filename);
        exit(errno);
    }

    return fuel;
}

