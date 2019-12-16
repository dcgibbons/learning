/*
 * day7.c
 * Advent of Code - 2019 - Day 7
 * https://adventofcode.com/2019/day/7
 *
 * Chad Gibbons
 * December 11, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "intcode.h"

#define AMPLIFIER_SIZE 5

/* forward reference */
static void permute(intcode_t intcode, long v[AMPLIFIER_SIZE],
        int start, int end, long* best_output);

int main(int argc, char* argv[])
{
    int rc = EXIT_SUCCESS;
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [intcode program]\n", argv[0]);
        rc = EXIT_FAILURE;
    } else {
        const char* input_filename = argv[1];
        intcode_t intcode = read_intcode(input_filename);
        if (intcode == NULL) {
            perror("unable to read intcode");
            rc = errno;
        } else {
            long best_output = -1;
            long arr[] = {0,1,2,3,4};
            permute(intcode, arr, 0, AMPLIFIER_SIZE, &best_output);
            printf("best output:%ld\n", best_output);
        }
    }
    return rc;
}

static long run_permutation(intcode_t intcode, long input[5])
{
    long  output = 0;
    for (int amplifier = 0; amplifier < 5; amplifier++) {
        long amplifier_input[2];
        amplifier_input[0] = input[amplifier];
        amplifier_input[1] = output;

        // reload the program each time
        intcode_t tmp_intcode = malloc(sizeof(*intcode));
        tmp_intcode->max_memory_size = intcode->max_memory_size;
        tmp_intcode->program_size = intcode->program_size;
        tmp_intcode->buffer = malloc(intcode->max_memory_size * sizeof(int));
        memcpy(tmp_intcode->buffer, intcode->buffer, intcode->max_memory_size * sizeof(int));

        run_intcode(tmp_intcode, amplifier_input, 2, &output);

        free(tmp_intcode->buffer);
        free(tmp_intcode);
    }

    return output;
}

static void swap(long* x, long* y)
{
    int tmp = *x;
    *x = *y;
    *y = tmp;
}

static void permute(intcode_t intcode, long v[AMPLIFIER_SIZE], 
        int start, int end, long* best_output)
{
    if (start == end) {
        int trial_output = run_permutation(intcode, v);
        if (trial_output > *best_output) {
            *best_output = trial_output;
        }
    } else {
        for (int i = start; i < end; i++) {
            swap(v + start, v + i);
            permute(intcode, v, start+1, end, best_output);
            swap(v + start, v + i);
        }
    }
}

