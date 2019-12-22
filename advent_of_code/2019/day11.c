/*
 * day11.c
 * Advent of Code 2019 - Day 11
 * https://adventofcode.com/2019/day/11
 *
 * Chad Gibbons
 * December 21, 2019
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "intcode.h"

#define MAP_SIZE 256

static char map[MAP_SIZE][MAP_SIZE];
static int painted[MAP_SIZE][MAP_SIZE];

/* forward reference */
static void emergency_hull_painting_robot(intcode_t intcode);

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
            emergency_hull_painting_robot(intcode);
            free_intcode(intcode);
        }
    }
    return EXIT_SUCCESS;
} 

static void emergency_hull_painting_robot(intcode_t intcode)
{
    memset(map, '.', sizeof(map));
    memset(painted, 0, sizeof(painted));

    int x = MAP_SIZE/2, y = MAP_SIZE/2;
    int robot_dir = 90;

    bool halted = false;
    while (!halted) {
        //print_map(map, x, y, robot_dir);

        long output = -1;
        long input = (map[y][x] == '.') ? 0 : 1;
        halted = run_intcode(intcode, &input, 1, &output);
        if (halted) break;

        map[y][x] = (output == 0) ? '.' : '#';
        painted[y][x]++;

        // run intcode again and determine where the robot should turn
        halted = run_intcode(intcode, &input, 1, &output);
        if (halted) break;

        robot_dir += (output == 0) ? 90 : -90;
        if (robot_dir < 0) robot_dir = 360 + robot_dir;
        if (robot_dir == 360) robot_dir = 0;

        // now move the robot 1 unit along the map
        switch (robot_dir) {
            case 0:         // right
                x++;
                break;
            case 90:        // up
                y--;
                break;
            case 180:       // left
                x--;
                break;
            case 270:       // down
                y++;
                break;
        }
        assert(x >= 0 && x < MAP_SIZE);
        assert(y >= 0 && y < MAP_SIZE);
    }

    int panels_painted = 0;
    for (int y = 0; y < MAP_SIZE; y++) {
        for (int x = 0; x < MAP_SIZE; x++) {
            if (painted[y][x] > 0) panels_painted++;
        }
    }
    printf("panels painted=%d\n", panels_painted);
}
