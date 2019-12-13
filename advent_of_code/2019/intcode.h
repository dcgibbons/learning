/*
 * intcode.h
 * Advent of Code - 2019 - Full Intcode Computer Model
 * https://adventofcode.com/2019/
 *
 * Chad Gibbons
 * December 11, 2019
 */

#pragma once

#include <stdbool.h>

typedef struct _intcode* intcode_t;
struct _intcode {
    size_t max_memory_size;
    size_t program_size;
    int* buffer;
    int ip;
};

extern intcode_t read_intcode(const char* input_filename);
extern bool run_intcode(intcode_t intcode, const int* input, int n, int* output);
extern void free_intcode(intcode_t intcode);
