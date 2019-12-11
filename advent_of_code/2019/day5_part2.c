/*
 * day5_part2.c
 * Advent of Code - 2019 - Day 5
 * https://adventofcode.com/2019/day/5
 *
 * Chad Gibbons
 * December 10, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INTCODE_BUFFER      100000

/* forward reference for internal functions */
static int read_intcode(const char* input_filename,
        int** intcode_data, size_t* intcode_size);

static void run_intcode(int* intcode_data, size_t intcode_size);

int main(int argc, char* argv[])
{
    int rc = EXIT_SUCCESS;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s [input data]\n", argv[0]);
        rc = EXIT_FAILURE;
    } else {
        const char* input_filename = argv[1];
        int* intcode = NULL;
        size_t intcode_size = 0;
        if (read_intcode(input_filename, &intcode, &intcode_size) < 0) {
            perror("unable to read intcode");
            rc = EXIT_FAILURE;
        } else {
            printf("initial intcode data:\n");
            for (int i = 0; i < intcode_size; i++) {
                printf("%d", intcode[i]);
                if (i < intcode_size-1) {
                    printf(",");
                }
            }
            puts("\n");

            run_intcode(intcode, intcode_size);
        }
    }

    return rc;
}

static int read_intcode(const char* input_filename,
        int** intcode_data, size_t* intcode_size)
{
    assert(input_filename != NULL);
    assert(intcode_data != NULL);
    assert(intcode_size != NULL);

    FILE* fp = fopen(input_filename, "r");
    if (fp == NULL) {
        perror(input_filename);
        return errno;
    }

    int tmp_size = 0;
    int* tmp_data = calloc(MAX_INTCODE_BUFFER, sizeof(int));
    if (!tmp_data) {
        return errno;
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
            tmp_data[tmp_size++] = (int)val;
            ptr = endptr;
        } while (endptr != NULL && *endptr != '\0' && tmp_size < MAX_INTCODE_BUFFER);
    }

    if (!feof(fp)) {
        perror(input_filename);
        return errno;
    }

    *intcode_data = tmp_data;
    *intcode_size = tmp_size;

    return 0;
}

static void run_intcode(int* intcode_data, size_t intcode_size)
{
    int ip = 0;
    while (ip < intcode_size) {
        int raw_opcode = intcode_data[ip++];
        printf("ip=%4.4d opcode=%4.4d ", ip-1, raw_opcode);
        printf("[+1]=%d [+2]=%d [+3]=%d ",
                intcode_data[ip], intcode_data[ip+1], intcode_data[ip+2]);
        int opcode = raw_opcode % 100;
        int param1_mode = raw_opcode / 100 % 10;
        int param2_mode = raw_opcode / 1000 % 10;
        int param3_mode = raw_opcode / 10000 % 10;
        switch (opcode) {
            case 1:  // add
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                printf("ADD");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(",%d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);
                printf(" => %d", dest);

                intcode_data[dest] = (param1_mode ? opa : intcode_data[opa]) + (param2_mode ? opb : intcode_data[opb]);
                printf("[%d]\n", intcode_data[dest]);
                break;
            }
            case 2:  // multiplies
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                printf("MUL ");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(",%d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);
                printf("=> %d", dest);
                intcode_data[dest] = 
                    (param1_mode ? opa : intcode_data[opa]) 
                    * (param2_mode ? opb : intcode_data[opb]);
                printf("[%d]\n", intcode_data[dest]);
                break;
            }
            case 3:  // input
            {
                char buffer[BUFSIZ];
                printf("Input: ");
                fflush(stdout);
                if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
                    abort();
                } else {
                    long val = strtol(buffer, NULL, 10);
                    int dest = intcode_data[ip++];
                    intcode_data[dest] = (int)val;
                    printf("Stored %ld in [%d]\n", val, dest);
                }
                break;
            }
            case 4:  // output
            {
                int dest = intcode_data[ip++];
                int value = (param1_mode ? dest : intcode_data[dest]);
                if (param1_mode) {
                    printf("value is %d [immediate]\n", value);
                } else {
                    printf("value at [%d]=%d\n", dest, value);
                }
                break;
            }
            case 5: // jump-if-true
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                printf("JUMP-IF-TRUE");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(", %d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);
                if ((param1_mode ? opa : intcode_data[opa])) {
                    ip = (param2_mode ? opb : intcode_data[opb]);
                    printf(" ip => %d", ip);
                }
                printf("\n");
                break;
            }
            case 6: // jump-if-false
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                printf("JMP-IF-FALSE");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(", %d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);
                if (!(param1_mode ? opa : intcode_data[opa])) {
                    ip = (param2_mode ? opb : intcode_data[opb]);
                    printf(" ip => %d", ip);
                }
                printf("\n");
                break;
            }
            case 7: // less than
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                printf("LT");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(", %d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);

                if ((param1_mode ? opa : intcode_data[opa]) < (param2_mode ? opb : intcode_data[opb])) {
                    intcode_data[dest] = 1;
                } else {
                    intcode_data[dest] = 0;
                }
                printf(" => %d[%d]\n", dest, intcode_data[dest]);
                break;
            }
            case 8: // equals
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                printf("EQ");
                printf(" %d", opa);
                if (!param1_mode) printf("[%d]", intcode_data[opa]);
                printf(", %d", opb);
                if (!param2_mode) printf("[%d]", intcode_data[opb]);

                if ((param1_mode ? opa : intcode_data[opa]) == (param2_mode ? opb : intcode_data[opb])) {
                    intcode_data[dest] = 1;
                } else {
                    intcode_data[dest] = 0;
                }
                printf(" => %d[%d]\n", dest, intcode_data[dest]);
                break;
            }
            case 99:
            {
                printf("HALT\n");
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

