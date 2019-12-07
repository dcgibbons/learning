/*
 * day2.c
 * Advent of Code - 2019 - Day 2
 * https://adventofcode.com/2019/day/2
 *
 * Chad Gibbons
 * December 6, 2019
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

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

            // per instructions: replace position 1 with 12 and position 2 with
            // 2
            intcode[1] = 12;
            intcode[2] = 2;

            run_intcode(intcode, intcode_size);

            printf("\nfinal intcode data:\n");
            for (int i = 0; i < intcode_size; i++) {
                printf("%d", intcode[i]);
                if (i < intcode_size-1) {
                    printf(",");
                }
            }
            puts("");
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
    while (ip < intcode_size) {
        int opcode = intcode_data[ip++];
        switch (opcode) {
            case 1:  // add
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                intcode_data[dest] = intcode_data[opa] + intcode_data[opb];
                break;
            }
            case 2:  // multiplies
            {
                int opa = intcode_data[ip++];
                int opb = intcode_data[ip++];
                int dest = intcode_data[ip++];
                intcode_data[dest] = intcode_data[opa] * intcode_data[opb];
                break;
            }
            case 99:
            {
                return;
            }
        }
    }
}

