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

    return intcode;
}

void run_intcode(intcode_t intcode, const int* input, int n, int* output)
{
    int ip = 0;
    int inputp = 0;

    while (ip < intcode->program_size) {
        int raw_opcode = intcode->buffer[ip++];
        int opcode = raw_opcode % 100;
        int param1_mode = raw_opcode / 100 % 10;
        int param2_mode = raw_opcode / 1000 % 10;
        // int param3_mode = raw_opcode / 10000 % 10;

        switch (opcode) {
            case 1:  // add
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                int dest = intcode->buffer[ip++];
                int r1 = (param1_mode ? opa : intcode->buffer[opa]);
                int r2 = (param2_mode ? opb : intcode->buffer[opb]);
                intcode->buffer[dest] = r1 + r2;
                break;
            }
            case 2:  // multiplies
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                int dest = intcode->buffer[ip++];
                int r1 = (param1_mode ? opa : intcode->buffer[opa]);
                int r2 = (param2_mode ? opb : intcode->buffer[opb]);
                intcode->buffer[dest] = r1 * r2;
                break;
            }
            case 3:  // input
            {
                if (inputp == n) abort();
                int val = input[inputp++];
                int dest = intcode->buffer[ip++];
                intcode->buffer[dest] = val;
                break;
            }
            case 4:  // output
            {
                int dest = intcode->buffer[ip++];
                int value = (param1_mode ? dest : intcode->buffer[dest]);
                *output = value;
                break;
            }
            case 5: // jump-if-true
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                if ((param1_mode ? opa : intcode->buffer[opa])) {
                    ip = (param2_mode ? opb : intcode->buffer[opb]);
                }
                break;
            }
            case 6: // jump-if-false
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                if (!(param1_mode ? opa : intcode->buffer[opa])) {
                    ip = (param2_mode ? opb : intcode->buffer[opb]);
                }
                break;
            }
            case 7: // less than
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                int dest = intcode->buffer[ip++];
                int r1 = (param1_mode ? opa : intcode->buffer[opa]);
                int r2 = (param2_mode ? opb : intcode->buffer[opb]);
                if (r1 < r2) {
                    intcode->buffer[dest] = 1;
                } else {
                    intcode->buffer[dest] = 0;
                }
                break;
            }
            case 8: // equals
            {
                int opa = intcode->buffer[ip++];
                int opb = intcode->buffer[ip++];
                int dest = intcode->buffer[ip++];
                int r1 = (param1_mode ? opa : intcode->buffer[opa]);
                int r2 = (param2_mode ? opb : intcode->buffer[opb]);
                if (r1 == r2) {
                    intcode->buffer[dest] = 1;
                } else {
                    intcode->buffer[dest] = 0;
                }
                break;
            }
            case 99:
            {
                //printf("HALT\n");
                return;
            }
            default:
            {
                puts("UNKNOWN INSTRUCTION ABORTING");
                abort();
            }
        }
    }
}

