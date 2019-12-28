/*
 * day15_part2.c
 * Advent of Code 2019 - Day 15, Part 2
 * https://adventofcode.com/2019/day/15#part2
 * 
 * Chad Gibbons
 * December 27, 2019
 *
 * For part 2, a simple depth-first-search algorithm is used to map the entire
 * ship area, and then Dijkstra is used to path find everywhere, and then the
 * maximum distance to anywhere on the map is how many seconds it takes to fill
 * the ship compartment with O2.
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "intcode.h"
#include "hash_table.h"
#include "priority_queue.h"

#define HASH_TABLE_SIZE         100
#define MAP_HEIGHT              42
#define MAP_WIDTH               42
#define MAP_SIZE                (MAP_HEIGHT * MAP_WIDTH)
#define MAP_XY(map, x, y)       (*(&(map)[(y)*MAP_WIDTH + (x)]))
#define MAP_IS_WALL(map, x, y)  (MAP_XY(map, x, y) == '#')
#define NORTH                   1
#define SOUTH                   2
#define WEST                    3
#define EAST                    4

typedef struct position {
    int16_t x;
    int16_t y;
} position_t;

#define POS_TO_INT(pos) (((pos).x << 16) | (pos).y)
#define INT_TO_POS(val) { .x = ((val) >> 16), .y = ((val) & 0xffff) }
#define XY_TO_INT(x,y)  (((x) << 16) | (y))

/* forward reference */
static char* create_map();
static void map_ship(intcode_t intcode, char* map, int* o2_x, int* o2_y);
static void print_map(char* map);
static void do_map(intcode_t intcode, char* map, int x, int y, int* o2_x, int* o2_y);
static int fill_ship(char* map, int start_x, int start_y);

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
            char* map = create_map();

            int o2_x, o2_y;
            map_ship(intcode, map, &o2_x, &o2_y);

            int elapsed = fill_ship(map, o2_x, o2_y);
            printf("%d seconds to fill ship\n", elapsed);

            free(map);
            free_intcode(intcode);
        }
    }
    return EXIT_SUCCESS;
} 

static char* create_map()
{
    const size_t bufsiz = MAP_SIZE * sizeof(char);
    char* map = malloc(bufsiz);
    if (!map) {
        fprintf(stderr, "Unable to allocate map buffer of %ld bytes\n", bufsiz);
        abort();
    }
    memset(map, ' ', bufsiz);
    return map;
}

static void do_movement(intcode_t intcode,
        char* map,
        int x,
        int y,
        int movement,
        int return_movement,
        int* o2_x,
        int* o2_y)
{
    long output = -1;
    long input = movement;
    run_intcode(intcode, &input, 1, &output);

    switch (output) {
        case 0:  // droid hit a wall and did not move
            MAP_XY(map, x, y) = '#';
            break;
        case 1:  // droid moved successfully
            MAP_XY(map, x, y) = '.';
            break;
        case 2:  // O2 system found
            MAP_XY(map, x, y) = 'O';
            *o2_x = x;
            *o2_y = y;
            break;
    }

    if (output != 0) {
        // recursively map from the new position
        do_map(intcode, map, x, y, o2_x, o2_y);

        // return to original position when called
        output = -1;
        input = return_movement;
        run_intcode(intcode, &input, 1, &output);
        assert(output > 0);
    }
}

static void do_map(intcode_t intcode,
        char* map,
        int x,
        int y,
        int* o2_x,
        int* o2_y)
{
    // check NORTH
    if (y > 0 && (MAP_XY(map, x, y-1) == ' ')) {
        do_movement(intcode, map, x, y-1, NORTH, SOUTH, o2_x, o2_y);
    }

    // check SOUTH
    if (y < MAP_HEIGHT && (MAP_XY(map, x, y+1) == ' ')) {
        do_movement(intcode, map, x, y+1, SOUTH, NORTH, o2_x, o2_y);
    }

    // check WEST
    if (x > 0 && (MAP_XY(map, x-1, y) == ' ')) {
        do_movement(intcode, map, x-1, y, WEST, EAST, o2_x, o2_y);
    }

    // check EAST
    if (x < MAP_WIDTH && (MAP_XY(map, x+1, y) == ' ')) {
        do_movement(intcode, map, x+1, y, EAST, WEST, o2_x, o2_y);
    }
}

static void map_ship(intcode_t intcode, char* map, int* o2_x, int* o2_y)
{
    int start_x = MAP_WIDTH/2, start_y = MAP_HEIGHT/2;
    do_map(intcode, map, start_x, start_y, o2_x, o2_y);
    print_map(map);
}

static void print_map(char* map)
{
    for (int y = 0; y < MAP_HEIGHT; y++) {
        for (int x = 0; x < MAP_WIDTH; x++) {
            printf("%c", MAP_XY(map,x,y));
        }
        printf("\n");
    }
    printf("\n");
}

bool can_move(char* map, const int x, const int y)
{
    if (x < 0 || x >= MAP_WIDTH) return false;
    if (y < 0 || y >= MAP_HEIGHT) return false;
    if (MAP_IS_WALL(map, x, y)) return false;
    return true;
}

static void init_find(char* map,
        hashtable_t* dist,
        hashtable_t* prev,
        queue_t** pqueue)
{
    for (int x = 0; x < MAP_WIDTH; x++) {
        for (int y = 0; y < MAP_HEIGHT; y++) {
            if (can_move(map, x, y)) {
                intptr_t key = XY_TO_INT(x, y);
                hash_insert(dist, (void*)key, (void*)(intptr_t)(MAP_SIZE));
                hash_insert(prev, (void*)key, NULL);
                queue_push(pqueue, MAP_SIZE, (void*)key);
            }
        }
    }
}

static void check_adjacent(char* map,
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

static int fill_ship(char* map, int start_x, int start_y)
{
    queue_t* pqueue = NULL;
    hashtable_t* dist = hash_init(HASH_TABLE_SIZE);
    hashtable_t* prev = hash_init(HASH_TABLE_SIZE);

    init_find(map, dist, prev, &pqueue);

    // update the distance from the start position to be 0 to start the search
    intptr_t start_key = XY_TO_INT(start_x, start_y);
    hash_insert(dist, (void*)start_key, (void*)0);
    queue_update_priority(&pqueue, 0, (void*)start_key);

    // do dijkstra!
    while (pqueue != NULL) {
        // get the next lowest distance node from the priority queue
        intptr_t ukey = (intptr_t)queue_pop(&pqueue);
        position_t pos = INT_TO_POS(ukey);
        intptr_t udist = (intptr_t)(hash_lookup(dist, (void*)ukey)->value);

        check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x, pos.y-1);
        check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x+1, pos.y);
        check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x, pos.y+1);
        check_adjacent(map, dist, prev, &pqueue, ukey, udist, pos.x-1, pos.y);
    }

    // HACK: find the max distance of all the nodes
    // TODO: add in a vistor pattern function to the hashtable
    int maxpathlen = 0;
    for (int h = 0; h < dist->table_size; h++) {
        for (hashnode_t* n = dist->table[h]; n != NULL; n = n->next) {
            position_t pos = INT_TO_POS((intptr_t)n->key);
            if (MAP_XY(map, pos.x, pos.y) != '#' && MAP_XY(map, pos.x, pos.y) != ' ') {
                int d = (intptr_t)n->value;
                if (maxpathlen < d) {
                        maxpathlen = d;
                }
            }
        }
    }

    hash_free(&dist);
    hash_free(&prev);
    queue_free(&pqueue);

    return maxpathlen;
}

