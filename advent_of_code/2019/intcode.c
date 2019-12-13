/*
 * intcode.c
 * Advent of Code - 2019 - Full Intcode Computer Model
 * https://adventofcode.com/2019/
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

#define MAX_INTCODE_BUFFER      100000

intcode_t read_intcode(const char* input_filename)
{
    FILE* fp = fopen(input_filename, "r");
    if (fp == NULL) {
        return NULL;
    }

    intcode_t intcode = malloc(sizeof(*intcode));
    if (!intcode) return NULL;
    intcode->max_memory_size = MAX_INTCODE_BUFFER;
    intcode->program_size = 0;
    intcode->ip = 0;
    intcode->buffer = calloc(intcode->max_memory_size, sizeof(int));
    if (!intcode->buffer) {
        free(intcode);
        return NULL;
    }

    // TODO: BUFSIZ big enough for input? no! craft elves made the input 
    // larger than BUFSIZ; NICE!
    char buffer[8192];  
    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        char* endptr = NULL;
        char* ptr = buffer;
        do {
            long val = strtol(ptr, &endptr, 10);
            if (*endptr == ',') endptr++;
            if (isspace(*endptr)) endptr++;
            intcode->buffer[intcode->program_size++] = (int)val;
            ptr = endptr;
        } while (endptr != NULL && *endptr != '\0' && intcode->program_size < MAX_INTCODE_BUFFER);
    }

    if (!feof(fp)) {
        free(intcode->buffer);
        free(intcode);
        return NULL;
    }

    fclose(fp);

    return intcode;
}

void free_intcode(intcode_t intcode)
{
    free(intcode->buffer);
    memset(intcode, 0xff, sizeof(*intcode));  // debugging safety
    free(intcode);
}

void add(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int dest = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    intcode->buffer[dest] = r1 + r2;
}

void mul(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int dest = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    intcode->buffer[dest] = r1 * r2;
}

void jump_if_true(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    if (r1 != 0) {
        intcode->ip = r2;
    }
}

void jump_if_false(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    if (r1 == 0) {
        intcode->ip = r2;
    }
}

void less_than(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int dest = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    if (r1 < r2) {
        intcode->buffer[dest] = 1;
    } else {
        intcode->buffer[dest] = 0;
    }
}

void equals(intcode_t intcode, int param1_mode, int param2_mode)
{
    int opa = intcode->buffer[intcode->ip++];
    int opb = intcode->buffer[intcode->ip++];
    int dest = intcode->buffer[intcode->ip++];
    int r1 = (param1_mode ? opa : intcode->buffer[opa]);
    int r2 = (param2_mode ? opb : intcode->buffer[opb]);
    if (r1 == r2) {
        intcode->buffer[dest] = 1;
    } else {
        intcode->buffer[dest] = 0;
    }
}

bool run_intcode(intcode_t intcode, const int* input, int n, int* output)
{
    int inputp = 0;
    while (intcode->ip < intcode->program_size) {
        int raw_opcode = intcode->buffer[intcode->ip++];
        int opcode = raw_opcode % 100;
        int param1_mode = raw_opcode / 100 % 10;
        int param2_mode = raw_opcode / 1000 % 10;
        // int param3_mode = raw_opcode / 10000 % 10;

        switch (opcode) {
            case 1:  // add
                add(intcode, param1_mode, param2_mode);
                break;
            case 2:  // multiplies
                mul(intcode, param1_mode, param2_mode);
                break;
            case 3:  // input
            {
                if (inputp == n) {
                    fprintf(stderr, "insufficient input data, inputp=%d n=%d\n",
                            inputp, n);
                    abort();  // no input left
                }
                int val = input[inputp++];
                int dest = intcode->buffer[intcode->ip++];
                intcode->buffer[dest] = val;
                break;
            }
            case 4:  // output
            {
                int dest = intcode->buffer[intcode->ip++];
                int value = (param1_mode ? dest : intcode->buffer[dest]);
                *output = value;
                return false;
            }
            case 5: // jump-if-true
                jump_if_true(intcode, param1_mode, param2_mode);
                break;
            case 6: // jump-if-false
                jump_if_false(intcode, param1_mode, param2_mode);
                break;
            case 7: // less than
                less_than(intcode, param1_mode, param2_mode);
                break;
            case 8: // equals
                equals(intcode, param1_mode, param2_mode);
                break;
            case 99:
                return true;
            default:
                fprintf(stderr, "UNKNOWN INSTRUCTION ABORTING\n");
                abort();
        }
    }

    return true;  // out of instructions, same as halted?
}

