/*
 * day17.c
 * Advent of Code 2019 - Day 17
 * https://adventofcode.com/2019/day/17
 * 
 * Chad Gibbons
 * December 29, 2019
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "intcode.h"

/* forward reference */
static long calibrate_cameras(intcode_t intcode);
static void print_map(const int height, const int width, char map[height][width]);

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
            long calibration = calibrate_cameras(intcode);
            free_intcode(intcode);
            printf("camera calibration=%ld\n", calibration);
        }
    }
    return EXIT_SUCCESS;
} 

static bool is_intersection(const int height, const int width,
        char map[height][width], const int x, const int y)
{
    return (map[y][x] != '.' &&
            (x > 0 && map[y][x-1] != '.') &&
            (x < width-1 && map[y][x+1] != '.') &&
            (y > 0 && map[y-1][x] != '.') &&
            (y < height-1 && map[y+1][x] != '.'));
}

static long calibrate_cameras(intcode_t intcode)
{
    const int map_width = 56;
    const int map_height = 36;
    char map[map_height][map_width];
    memset(map, '*', sizeof(map));

    bool halted = false;
    int x = 0, y = 0;
    while (!halted) {
        long output;
        halted = run_intcode(intcode, NULL, 0, &output);
        if (halted) continue;

        assert(y < map_height);
        assert(x < map_width);
        map[y][x] = (char)output;

        if (output == '\n') {
            y++;
            x = 0;
        } else {
            x++;
        }
    }
    print_map(map_height-1, map_width, map);

    long calibration = 0;
    for (int y = 0; y < map_height; y++) {
        for (int x = 0; x < map_width; x++) {
            if (is_intersection(map_height-1, map_width, map, x, y)) {
                calibration += (x * y);
                map[y][x] = 'O';
            }
        }
    }
    print_map(map_height-1, map_width, map);
    return calibration;
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

