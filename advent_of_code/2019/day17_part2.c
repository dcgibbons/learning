/*
 * day17.c
 * Advent of Code 2019 - Day 17, Part 2
 * https://adventofcode.com/2019/day/17#part2
 * 
 * Chad Gibbons
 * December 29, 2019
 *
 * And on the 17th day, there came the total hacks.
 *
 * This one isn't solved 100% by automation, but it could be. The general
 * approach here is to use part 1 to build the map into a memory buffer. Then, a
 * simple path finding algorithm is used (greedy - go as far in any direction as
 * you can before turning). That results in a path command sequence like this,
 * for my particular puzzle input:
 *
 * L,10,R,12,R,12,R,6,R,10,L,10,L,10,R,12,R,12,R,10,L,10,L,12,R,6,R,6,R,10,L,10,R,10,L,10,L,12,R,6,R,6,R,10,L,10,R,10,L,10,L,12,R,6,L,10,R,12,R,12,R,10,L,10,L,12,
 *
 * The next approach is to find 3 common substrings in the command sequence,
 * each under 20 characters total, that can be used to construct the entire
 * path. A dictionary approach is likely the best way to automate this (ala
 * https://en.wikipedia.org/wiki/LZ77_and_LZ78), but I noticed a couple of
 * quirks that would make this challenging to completely automate:
 *   1. It's not precisely clear what the path end is (other than not
 *   back-tracking and being stuck).
 *   2. For my particular puzzle input, the only command sequence that worked
 *   includes an additional turn and move after the path end is reached. This
 *   works for this particular robot implementation, as it halts itself once it
 *   reaches the end point. 
 *
 * This was also the first time I found a C implementation of the intcode
 * computer to be in the way... some of the algorithms used within would have
 * been trivial in C++ or Python and were just more work here to try out.
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "intcode.h"

/* forward reference */
static void build_map(intcode_t intcode, const int map_height,
        const int map_width, char map[map_height][map_width],
        int* robot_x, int* robot_y, const char* input);

static void print_map(const int height, const int width,
        char map[height][width]);

static void find_path(const int height, const int width,
        char map[height][width], int robot_x, int robot_y);


int main(int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [input filename]\n", argv[0]);
        exit(EXIT_FAILURE);
    } else {
        const char* input_filename = argv[1];
        intcode_t intcode = read_intcode(input_filename);
        if (intcode == NULL) {
            perror("unable to read intcode");
            exit(errno);
        } else {
            const int map_width = 56;
            const int map_height = 36;
            char map[map_height][map_width];
            memset(map, 0, sizeof(map));

            int robot_x, robot_y;
            build_map(intcode, map_height, map_width, map, &robot_x, &robot_y, NULL);
            free_intcode(intcode);

            print_map(map_height, map_width, map);
            find_path(map_height, map_width, map, robot_x, robot_y);

            memset(map, 0, sizeof(map));
            const char* prg = "A,B,A,C,B,C,B,C,A,C\n" 
                              "L,10,R,12,R,12\n"
                              "R,6,R,10,L,10\n"
                              "R,10,L,10,L,12,R,6\n"
                              "y\n";
            intcode = read_intcode(input_filename);
            build_map(intcode, map_height, map_width, map, &robot_x, &robot_y, prg);
            free_intcode(intcode);
        }
    }
    return EXIT_SUCCESS;
} 

static void build_map(intcode_t intcode,
        const int map_height, const int map_width,
        char map[map_height][map_width],
        int* robot_x, int* robot_y,
        const char* input)
{
    long* input_buffer = NULL;
    long input_size = 0;

    if (input != NULL) {
        assert(intcode->buffer[0] == 1);
        intcode->buffer[0] = 2;

        input_size = strlen(input);
        input_buffer = malloc(sizeof(int) * input_size);
        for (int i = 0; i < input_size; i++) {
            input_buffer[i] = (long)input[i];
            putchar(input[i]);
        }
    }

    bool halted = false;
    int x = 0, y = 0;
    while (!halted) {
        long output;
        halted = run_intcode(intcode, input_buffer, input_size, &output);
        if (halted) {
            printf("HLT - final output: %ld\n", output);
            continue;
        }

        // only add the data to our map buffer if within bounds, otherwise
        // it's just output data for the terminal
        if (y < map_height && x < map_width) {
            map[y][x] = (char)output;
        }

        if (input_buffer == NULL) {
            switch (output) {
                case '\n':
                    y++;
                    x = 0;
                    break;
                case '^':
                case 'v':
                case '<':
                case '>':
                    *robot_x = x;
                    *robot_y = y;
                    // fall through!
                default:
                    x++;
            }
        } else {
            putchar(output);
        }
    }

    if (input_buffer != NULL) {
        free(input_buffer);
    }
}

static void print_map(const int height, const int width, char map[height][width])
{
    for (int y = 0; y < height; y++) {
        printf("%02d ", y);
        for (int x = 0; x < width; x++) {
            printf("%c", map[y][x]);
        }
    }
    printf("\n\n");
}

static void find_path(const int height, const int width,
        char map[height][width],
        int robot_x, int robot_y)
{
    int end_x = 54, end_y = 30; // HACK! this makes the termination case easy?

    char movements[BUFSIZ];
    memset(movements, 0, sizeof(movements));
    char* m = movements;

    while (robot_x != end_x && robot_y != end_y) {
        char robot_dir = map[robot_y][robot_x];
        // holy lazy hacker mode...
        switch (robot_dir) {
            case '^':
            case 'v':
                if (robot_x > 0 && map[robot_y][robot_x-1] == '#') {
                    *m++ = (robot_dir == '^') ? 'L' : 'R';
                    *m++ = ',';
                    int x0 = robot_x-1;
                    while (x0 >= 0 && map[robot_y][x0] == '#') {
                        x0--;
                    }
                    x0++;
                    int diff = robot_x-x0;
                    m += snprintf(m, BUFSIZ-(movements-m), "%d,", diff);
                    map[robot_y][robot_x] = '#';
                    robot_x = x0;
                    map[robot_y][robot_x] = '<';
                } else if (robot_x < width-1 && map[robot_y][robot_x+1] == '#') {
                    *m++ = (robot_dir == '^') ? 'R' : 'L';
                    *m++ = ',';
                    int x0 = robot_x+1;
                    while (x0 < width && map[robot_y][x0] == '#') {
                        x0++;
                    }
                    x0--;
                    int diff = x0-robot_x;
                    m += snprintf(m, BUFSIZ-(movements-m), "%d,", diff);
                    map[robot_y][robot_x] = '#';
                    robot_x = x0;
                    map[robot_y][robot_x] = '>';
                } else {
                    abort();
                }
                break;
            case '<':
            case '>':
                if (robot_y > 0 && map[robot_y-1][robot_x] == '#') {
                    *m++ = (robot_dir == '<') ? 'R' : 'L';
                    *m++ = ',';
                    int y0 = robot_y-1;
                    while (y0 >= 0 && map[y0][robot_x] == '#') {
                        y0--;
                    }
                    y0++;
                    int diff = robot_y-y0;
                    m += snprintf(m, BUFSIZ-(movements-m), "%d,", diff);
                    map[robot_y][robot_x] = '#';
                    robot_y = y0;
                    map[robot_y][robot_x] = '^';
                } else if (robot_y < height-1 && map[robot_y+1][robot_x] == '#') {
                    *m++ = (robot_dir == '<') ? 'L' : 'R';
                    *m++ = ',';
                    int y0 = robot_y+1;
                    while (y0 < height && map[y0][robot_x] == '#') {
                        y0++;
                    }
                    y0--;
                    int diff = y0-robot_y;
                    m += snprintf(m, BUFSIZ-(movements-m), "%d,", diff);
                    map[robot_y][robot_x] = '#';
                    robot_y = y0;
                    map[robot_y][robot_x] = 'v';
                } else {
                    abort();
                }
                break;
        }
    }
    printf("final movements=%s\n", movements);
}

