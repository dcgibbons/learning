/*
 * day7_part2.c
 * Advent of Code - 2019 - Day 7
 * https://adventofcode.com/2019/day/7
 *
 * Chad Gibbons
 * December 12, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "intcode.h"

#define NUM_AMPLIFIERS 5

/* forward reference */
static void permute(const char* input_filename,
                    int v[NUM_AMPLIFIERS],
                    int start, int end, int* best_output);

int main(int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [intcode program]\n", argv[0]);
        exit(EXIT_FAILURE);
    } else {
        const char* input_filename = argv[1];
        int best_output = -1;
        int arr[] = {5,6,7,8,9};
        permute(input_filename, arr, 0, NUM_AMPLIFIERS, &best_output);
        printf("best output:%d\n", best_output);
    }
    return EXIT_SUCCESS;
}

static int run_permutation(intcode_t amplifiers[NUM_AMPLIFIERS],
                           int input[NUM_AMPLIFIERS])
{
    int output = 0;

    // first pass!
    for (int amplifier = 0; amplifier < NUM_AMPLIFIERS; amplifier++) {
        int amplifier_input[2];
        amplifier_input[0] = input[amplifier];
        amplifier_input[1] = output;
        (void)run_intcode(amplifiers[amplifier], amplifier_input, 2, &output);
    }

    // remaining passes - go until halt!
    bool halted = false;
    while (!halted) {
        for (int amplifier = 0; amplifier < NUM_AMPLIFIERS; amplifier++) {
            halted = run_intcode(amplifiers[amplifier], &output, 1, &output);
        }
    }

    return output;
}

static void swap(int* x, int* y)
{
    int tmp = *x;
    *x = *y;
    *y = tmp;
}

static void startup_amplifiers(const char* intcode_filename, 
                               intcode_t amplifiers[NUM_AMPLIFIERS])
{
    // read the intcode program again to get a fresh computer state
    for (int amplifier = 0; amplifier < NUM_AMPLIFIERS; amplifier++) {
        intcode_t intcode = read_intcode(intcode_filename);
        if (intcode == NULL) {
            perror("unable to read intcode");
            exit(errno);
        } else {
            amplifiers[amplifier] = intcode;
        }
    }
}

static void shutdown_amplifiers(intcode_t amplifiers[NUM_AMPLIFIERS])
{
    // free the intcode computer memory
    for (int amplifier = 0; amplifier < NUM_AMPLIFIERS; amplifier++) {
        free_intcode(amplifiers[amplifier]);
    }
}

static void permute(const char* input_filename,
                    int v[NUM_AMPLIFIERS],
                    int start, int end, int* best_output)
{
    // base case - we are at the of a permutation, so check it!
    if (start == end) {
        intcode_t amplifiers[NUM_AMPLIFIERS];
        startup_amplifiers(input_filename, amplifiers);

        int trial_output = run_permutation(amplifiers, v);
        if (trial_output > *best_output) {
            *best_output = trial_output;
        }

        shutdown_amplifiers(amplifiers);

    } else {
        // perform all sub-permutations of this permutation
        for (int i = start; i < end; i++) {
            swap(v + start, v + i);
            permute(input_filename, v, start+1, end, best_output);
            swap(v + start, v + i);
        }
    }
}

