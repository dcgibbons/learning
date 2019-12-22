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

#define MAX_INTCODE_BUFFER      (64 * 1024 * 1024)

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
    intcode->relative_base = 0;
    intcode->buffer = calloc(intcode->max_memory_size, sizeof(long));
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
            intcode->buffer[intcode->program_size++] = val;
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

long get_operand(intcode_t intcode, long param_mode)
{
    long opa = intcode->buffer[intcode->ip++];
    long operand;
    switch (param_mode) {
        case 0:
            operand = intcode->buffer[opa];
            break;
        case 1:
            operand = opa;
            break;
        case 2:
            operand = intcode->buffer[intcode->relative_base + opa];
            break;
        default:
            abort();
    }
    return operand;
}

void add(intcode_t intcode, int p1_mode, int p2_mode, int p3_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    long val = r1 + r2;

    long dest = intcode->buffer[intcode->ip++];
    switch (p3_mode) {
        case 0:
            assert(dest >= 0 && dest < MAX_INTCODE_BUFFER);
            intcode->buffer[dest] = val;
            break;
        case 2:
            assert(intcode->relative_base+dest >= 0 && intcode->relative_base+dest < MAX_INTCODE_BUFFER);
            intcode->buffer[intcode->relative_base + dest] = val;
            break;
        default:
            abort();
    }
    //printf("ADD %ld+%ld = -> %ld[%ld]\n", r1, r2, dest, intcode->buffer[dest]);
}

void mul(intcode_t intcode, int p1_mode, int p2_mode, int p3_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    long val = r1 * r2;

    long dest = intcode->buffer[intcode->ip++];
    switch (p3_mode) {
        case 0:
            assert(dest >= 0 && dest < MAX_INTCODE_BUFFER);
            intcode->buffer[dest] = val;
            break;
        case 2:
            assert(intcode->relative_base+dest >= 0 && intcode->relative_base+dest < MAX_INTCODE_BUFFER);
            intcode->buffer[intcode->relative_base + dest] = val;
            break;
        default:
            abort();
    }
}

void jump_if_true(intcode_t intcode, int p1_mode, int p2_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    if (r1 != 0) {
        intcode->ip = r2;
    }
}

void jump_if_false(intcode_t intcode, int p1_mode, int p2_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    if (r1 == 0) {
        intcode->ip = r2;
    }
}

void less_than(intcode_t intcode, int p1_mode, int p2_mode, int p3_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    long val = (r1 < r2) ? 1 : 0;

    long dest = intcode->buffer[intcode->ip++];
    switch (p3_mode) {
        case 0:
            assert(dest >= 0 && dest < MAX_INTCODE_BUFFER);
            intcode->buffer[dest] = val;
            break;
        case 2:
            assert(intcode->relative_base+dest >= 0 && intcode->relative_base+dest < MAX_INTCODE_BUFFER);
            intcode->buffer[intcode->relative_base + dest] = val;
            break;
        default:
            abort();
    }
    //printf("LT %ld<%ld = -> %ld[%ld]\n", r1, r2, dest, intcode->buffer[dest]);
}

void equals(intcode_t intcode, int p1_mode, int p2_mode, int p3_mode)
{
    long r1 = get_operand(intcode, p1_mode);
    long r2 = get_operand(intcode, p2_mode);
    long val = (r1 == r2) ? 1 :0;

    long dest = intcode->buffer[intcode->ip++];
    switch (p3_mode) {
        case 0:
            assert(dest >= 0 && dest < MAX_INTCODE_BUFFER);
            intcode->buffer[dest] = val;
            break;
        case 2:
            assert(intcode->relative_base+dest >= 0 && intcode->relative_base+dest < MAX_INTCODE_BUFFER);
            intcode->buffer[intcode->relative_base + dest] = val;
            break;
        default:
            abort();
    }
    //printf("LT %ld==%ld = -> %ld[%ld]\n", r1, r2, dest, intcode->buffer[dest]);
}

bool run_intcode(intcode_t intcode, const long* input, long n, long* output)
{
    long inputp = 0;
    while (intcode->ip < intcode->program_size) {
        long raw_opcode = intcode->buffer[intcode->ip++];
        long opcode = raw_opcode % 100;
        long p1_mode = raw_opcode / 100 % 10;
        long p2_mode = raw_opcode / 1000 % 10;
        int p3_mode = raw_opcode / 10000 % 10;

        switch (opcode) {
            case 1:  // add
                add(intcode, p1_mode, p2_mode, p3_mode);
                break;
            case 2:  // multiplies
                mul(intcode, p1_mode, p2_mode, p3_mode);
                break;
            case 3:  // input
            {
                if (inputp == n) {
                    fprintf(stderr, "insufficient input data, inputp=%ld n=%ld\n",
                            inputp, n);
                    abort();  // no input left
                }
                long val = input[inputp++];
                long dest = intcode->buffer[intcode->ip++];
                switch (p1_mode) {
                    case 0:
                        assert(dest >= 0 && dest < MAX_INTCODE_BUFFER);
                        intcode->buffer[dest] = val;
                        break;
                    case 2:
                        assert(intcode->relative_base+dest >= 0 && intcode->relative_base+dest < MAX_INTCODE_BUFFER);
                        intcode->buffer[intcode->relative_base + dest] = val;
                        break;
                    default:
                        abort();
                }
                break;
            }
            case 4:  // output
                *output = get_operand(intcode, p1_mode);
                return false;
            case 5: // jump-if-true
                jump_if_true(intcode, p1_mode, p2_mode);
                break;
            case 6: // jump-if-false
                jump_if_false(intcode, p1_mode, p2_mode);
                break;
            case 7: // less than
                less_than(intcode, p1_mode, p2_mode, p3_mode);
                break;
            case 8: // equals
                equals(intcode, p1_mode, p2_mode, p3_mode);
                break;
            case 9: // adjust relative base
                intcode->relative_base += get_operand(intcode, p1_mode);
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

