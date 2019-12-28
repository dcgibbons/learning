/*
 * day15.c
 * Advent of Code 2019 - Day 15
 * https://adventofcode.com/2019/day/15
 * 
 * Chad Gibbons
 * December 26, 2019
 *
 * This approach lets the droid run randomly around the map until it finds
 * the O2 system. Then after that, a shortest path clagorithm (Dijkstra) is used
 * to calculate the minimum path to get from the droid's origin to that point.
 *
 * Problems and thoughts on this solution:
 *   1. the droid will randomly find the O2 sensor, so it may not have fully
 *   explored the map. This can lead to an incorrect (too small) of an answer
 *   because the map is not fully populated.
 *   2. Dijkstra is overkill for just finding the length of the shorest path
 *   (and not the path itself), but it is still easy to code.
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "intcode.h"
#include "hash_table.h"
#include "priority_queue.h"

#define HASH_TABLE_SIZE 10
#define MAP_SIZE        100
#define NORTH           1
#define SOUTH           2
#define WEST            3
#define EAST            4

#define MAP_IS_END(map, x, y)   (map[y][x] == 'O')
#define MAP_IS_WALL(map, x, y)  (map[y][x] == '#')

typedef struct position {
    int16_t x;
    int16_t y;
} position_t;

#define POS_TO_INT(pos) (((pos).x << 16) | (pos).y)
#define INT_TO_POS(val) { .x = ((val) >> 16), .y = ((val) & 0xffff) }
#define XY_TO_INT(x,y)  (((x) << 16) | (y))

static char map[MAP_SIZE][MAP_SIZE]; // allocate in bss, not stack

/* forward reference */
static void repair_droid(intcode_t intcode);
static int find_path(char map[MAP_SIZE][MAP_SIZE], int start_x, int start_y);

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
            repair_droid(intcode);
            free_intcode(intcode);
        }
    }
    return EXIT_SUCCESS;
} 

static void repair_droid(intcode_t intcode)
{
    srand(time(NULL));

    memset(map, ' ', sizeof(map));

    int start_x = MAP_SIZE/2, start_y = MAP_SIZE/2;
    int x = start_x, y = start_y;
    int o2_x = -1, o2_y = -1;

    bool halted = false;
    int steps = 0;
    long input = 0;
    long output = -1;
    while (!halted) {
        steps++;

        input = -1;
        int next_input;
        do {
            next_input = (rand() % 4) + 1;
            switch (next_input) {
                case NORTH:
                    if (y > 0 && (map[y-1][x] == ' ' || map[y-1][x] == '.')) {
                        input = next_input;
                    }
                    break;
                case SOUTH:
                    if (y < MAP_SIZE && (map[y+1][x] == ' ' || map[y+1][x] == '.')) {
                        input = next_input;
                    }
                    break;
                case WEST:
                    if (x > 0 && (map[y][x-1] == ' ' || map[y][x-1] == '.')) {
                        input = next_input;
                    }
                    break;
                case EAST:
                    if (x < MAP_SIZE && (map[y][x+1] == ' ' || map[y][x+1] == '.')) {
                        input = next_input;
                    }
                    break;
            }
        } while (input == -1);

        halted = run_intcode(intcode, &input, 1, &output);
        if (halted) break;

        switch (output)  {
            case 0:  // droid hit a wall
                switch (input) {
                    case NORTH:
                        map[y-1][x] = '#';
                        break;
                    case SOUTH:
                        map[y+1][x] = '#';
                        break;
                    case WEST:
                        map[y][x-1] = '#';
                        break;
                    case EAST:
                        map[y][x+1] = '#';
                        break;
                    default:
                        abort();
                }
                break;
            case 1:  // droid moved in requested direction
            case 2:  // droid moved in requested direction and found O2 system
                switch (input) {
                    case NORTH:
                        y--;
                        assert(y >= 0);  // intcode won't wrap around - so map is too small if this happnens
                        break;
                    case SOUTH:
                        y++;
                        assert(y < MAP_SIZE);
                        break;
                    case WEST:
                        x--;
                        assert(x >= 0);
                        break;
                    case EAST:
                        x++;
                        assert(x < MAP_SIZE);
                        break;
                    default:
                        abort();
                }
                if (output == 2) {
                    map[y][x] = 'O';
                    halted = true;
                    o2_x = x, o2_y = y;
                } else {
                    map[y][x] = '.';
                }
                break;
            default:
                abort();
        }
    }

    int shortest_path_size = find_path(map, start_x, start_y);
    printf("shortest path to exit: %d\n", shortest_path_size);
}

bool can_move(char map[MAP_SIZE][MAP_SIZE], const int x, const int y)
{
    if (x < 0 || x >= MAP_SIZE) return false;
    if (y < 0 || y >= MAP_SIZE) return false;
    if (MAP_IS_WALL(map, x, y)) return false;
    return true;
}

static void init_find(char map[MAP_SIZE][MAP_SIZE],
        hashtable_t* dist,
        hashtable_t* prev,
        queue_t** pqueue)
{
    for (int x = 0; x < MAP_SIZE; x++) {
        for (int y = 0; y < MAP_SIZE; y++) {
            if (can_move(map, x, y)) {
                intptr_t key = XY_TO_INT(x, y);
                hash_insert(dist, (void*)key, (void*)(intptr_t)(MAP_SIZE*MAP_SIZE));
                hash_insert(prev, (void*)key, NULL);
                queue_push(pqueue, MAP_SIZE*MAP_SIZE, (void*)key);
            }
        }
    }
}

static void check_adjacent(char map[MAP_SIZE][MAP_SIZE],
        hashtable_t* dist,
        hashtable_t* prev,
        queue_t** pqueue,
        intptr_t u_key,
        int u_dist,
        const int x,
        const int y)
{
    // if we can successfully move to the specified location, then...
    if (can_move(map, x, y)) {
        // calculate the new possible distance (distance of u + 1 more hop)
        // and then compare that with this node's distance to see if it's
        // closer or not
        int new_dist = u_dist + 1;
        position_t pos = {.x = x, .y = y};
        intptr_t v_key = POS_TO_INT(pos);
        intptr_t v_dist = (intptr_t)(hash_lookup(dist, (void*)v_key)->value);

        // it's closer! update the distance and predecessor of this node
        if (new_dist < v_dist) {
            hash_insert(dist, (void*)v_key, (void*)(intptr_t)(new_dist));
            hash_insert(prev, (void*)v_key, (void*)u_key);
            queue_update_priority(pqueue, new_dist, (void*)v_key);
        }
    }
}

static int find_path(char map[MAP_SIZE][MAP_SIZE], int start_x, int start_y)
{
    queue_t* pqueue = NULL;
    hashtable_t* dist = hash_init(HASH_TABLE_SIZE);
    hashtable_t* prev = hash_init(HASH_TABLE_SIZE);

    init_find(map, dist, prev, &pqueue);

    // update the distance from the start position to be 0 to start the search
    intptr_t start_key = XY_TO_INT(start_x, start_y);
    hash_insert(dist, (void*)start_key, (void*)0);
    queue_update_priority(&pqueue, 0, (void*)start_key);

    int pathlen = 0;

    // do dijkstra!
    while (pqueue != NULL) {
        // get the next lowest distance node from the priority queue
        intptr_t ukey = (intptr_t)queue_pop(&pqueue);
        position_t pos = INT_TO_POS(ukey);
        intptr_t udist = (intptr_t)(hash_lookup(dist, (void*)ukey)->value);

        // we found our goal! end the search
        if (MAP_IS_END(map, pos.x, pos.y)) {
            position_t end_pos = INT_TO_POS(ukey);
            pathlen = udist;
            break;
        } else {
            check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x, pos.y-1);
            check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x+1, pos.y);
            check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x, pos.y+1);
            check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x-1, pos.y);
        }
    }

    hash_free(&dist);
    hash_free(&prev);
    queue_free(&pqueue);

    return pathlen;
}

