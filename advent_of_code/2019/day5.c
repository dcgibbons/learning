/*
 * day5.c
 * Advent of Code - 2019 - Day 5
 * https://adventofcode.com/2019/day/5
 *
 * Chad Gibbons
 * December 9, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INTCODE_BUFFER      1000

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

    char buffer[BUFSIZ];  // TODO: BUFSIZ big enough for input? hmm
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
    printf("ip=%d intcode_data=%p intcode_size=%lu\n", ip, intcode_data, intcode_size);
    while (ip < intcode_size) {
        int raw_opcode = intcode_data[ip++];
        printf("raw_opcode=%d\n", raw_opcode);
        int opcode = raw_opcode % 100;
        int param1_mode = raw_opcode / 100 % 10;
        int param2_mode = raw_opcode / 1000 % 10;
        int param3_mode = raw_opcode / 10000 % 10;
        switch (opcode) {
            case 1:  // add
            {
                assert(param3_mode == 0);
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                intcode_data[dest] = 
                    (param1_mode ? opa : intcode_data[opa]) 
                    + (param2_mode ? opb : intcode_data[opb]);
                break;
            }
            case 2:  // multiplies
            {
                assert(param3_mode == 0);
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                intcode_data[dest] = 
                    (param1_mode ? opa : intcode_data[opa]) 
                    * (param2_mode ? opb : intcode_data[opb]);
                break;
            }
            case 3:  // input
            {
                assert(param1_mode == 0);
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
                //assert(param1_mode == 0);
                int dest = intcode_data[ip++];
                int value = intcode_data[dest];
                printf("value at [%d]=%d\n", dest, value);
                break;
            }
            case 99:
            {
                printf("halt\n");
                return;
            }
        }
    }
}

