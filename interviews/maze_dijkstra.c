/*
 * maze_bfs.c
 * interview question - is there an a path through the maze?
 */

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "hash_table.h"
#include "priority_queue.h"

#define HASH_TABLE_SIZE           10

#define MAZE_IS_START(maze, x, y) (maze[y][x] == 's')
#define MAZE_IS_END(maze, x, y)   (maze[y][x] == 'e')
#define MAZE_IS_WALL(maze, x, y)  (maze[y][x] == 'X')

#define MAZE_ROWS 10
#define MAZE_COLS 32
#define MAZE_SIZE (MAZE_ROWS * MAZE_COLS)
static char MAZE[MAZE_ROWS][MAZE_COLS] = {
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
    "X      XX                      X",
    "XX  XX       X XX        X     X",
    "X  X   X      XXXX   X  X X  X X",
    "XsXX   X   X X X     X    X  X X",
    "X   XX    X X    X   XX XXXX X X",
    "X   XX XXX  X  X  X  X  X eX  XX",
    "X   XX  XXX   X    XX  XX X X  X",
    "X   XX                        XX",
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
};

typedef struct position {
    int16_t x;
    int16_t y;
} position_t;

#define POS_TO_INT(pos) (((pos).x << 16) | (pos).y)
#define INT_TO_POS(val) { .x = ((val) >> 16), .y = ((val) & 0xffff) }
#define XY_TO_INT(x,y)  (((x) << 16) | (y))

bool can_move(char maze[MAZE_ROWS][MAZE_COLS], const int x, const int y)
{
    if (x < 0 || x >= MAZE_COLS) return false;
    if (y < 0 || y >= MAZE_ROWS) return false;
    if (MAZE_IS_WALL(maze, x, y)) return false;
    return true;
}

void check_adjacent(char maze[MAZE_ROWS][MAZE_COLS], hashtable_t* dist, hashtable_t* prev,
        queue_t** pqueue,
        intptr_t u_key, int u_dist, const int x, const int y)
{
    // if we can successfully move to the specified location, then...
    if (can_move(maze, x, y)) {
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

void init_find(char maze[MAZE_ROWS][MAZE_COLS], hashtable_t* dist, hashtable_t* prev, queue_t** pqueue)
{
    int k = 0;
    for (int x = 0; x < MAZE_COLS; x++) {
        for (int y = 0; y < MAZE_ROWS; y++) {
            // only add possible _movable_ squares to the queue
            if (can_move(maze, x, y)) {
                intptr_t key = XY_TO_INT(x, y);
                hash_insert(dist, (void*)key, (void*)(intptr_t)MAZE_SIZE);
                hash_insert(prev, (void*)key, NULL);
                queue_push(pqueue, MAZE_SIZE, (void*)key);
            }
            k++;
        }
    }
    assert(k == MAZE_SIZE);
}

int calc_path(char maze[MAZE_ROWS][MAZE_COLS],
        position_t end_pos,
        position_t* pathbuf,
        size_t pathbuflen,
        hashtable_t* dist,
        hashtable_t* prev)
{
    // now see what the distance between source -> end was!
    // S ← empty sequence
    // 2  u ← target
    // 3  if prev[u] is defined or u = source:          // Do something only if the vertex is reachable
    // 4      while u is defined:                       // Construct the shortest path with a stack S
    // 5          insert u at the beginning of S        // Push the vertex onto the stack
    // 6          u ← prev[u]                           // Traverse from target to source

    intptr_t end_key = POS_TO_INT(end_pos);
    hashnode_t* stack[MAZE_SIZE];
    memset(stack, 0, sizeof(stack));

    int si = 0;
    hashnode_t* prevnode = hash_lookup(prev, (void*)end_key);
    if (prevnode != NULL) {  // do something only if the vertex is reachable
        // walk the path backwards from the end and push onto a stack
        while (prevnode != NULL) {
            stack[si++] = prevnode;
            intptr_t prev_node_key = (intptr_t)(prevnode->value);
            prevnode = hash_lookup(prev, (void*)prev_node_key);
            assert(si < MAZE_SIZE);
        }

        // pop the stack to get the path in the proper order
        int j = 0;
        for (int i = si-1; i >= 0 && j < pathbuflen; i--, j++) {
            position_t pos = INT_TO_POS((intptr_t)(stack[i]->key));
            pathbuf[j].x = pos.x;
            pathbuf[j].y = pos.y;
        }
        // path of one means we never found the end...
        return (j == 1) ? 0 : j; 
    } else {
        return 0;
    }
}

int find_path(char maze[MAZE_ROWS][MAZE_COLS], position_t* pathbuf, size_t pathbuflen,
        const int source_x, const int source_y)
{
    queue_t* pqueue = NULL;
    hashtable_t* dist = hash_init(HASH_TABLE_SIZE);
    hashtable_t* prev = hash_init(HASH_TABLE_SIZE);

    init_find(maze, dist, prev, &pqueue);

    // update the distance from the source position to be 0 to start the search
    intptr_t source_key = XY_TO_INT(source_x, source_y);
    hash_insert(dist, (void*)source_key, (void*)0);
    queue_update_priority(&pqueue, 0, (void*)source_key);

    int pathlen = 0;

    // do dijkstra!
    while (pqueue != NULL) {
        // get the next lowest distance node from the priority queue
        intptr_t ukey = (intptr_t)queue_pop(&pqueue);
        position_t pos = INT_TO_POS(ukey);

        intptr_t udist = (intptr_t)(hash_lookup(dist, (void*)ukey)->value);

        check_adjacent(maze, dist, prev, &pqueue, ukey, udist, pos.x, pos.y-1);
        check_adjacent(maze, dist, prev, &pqueue, ukey, udist, pos.x+1, pos.y);
        check_adjacent(maze, dist, prev, &pqueue, ukey, udist, pos.x, pos.y+1);
        check_adjacent(maze, dist, prev, &pqueue, ukey, udist, pos.x-1, pos.y);

        // we found our goal! end the search
        if (MAZE_IS_END(maze, pos.x, pos.y)) {
            position_t end_pos = INT_TO_POS(ukey);
            pathlen = calc_path(maze, end_pos, pathbuf, pathbuflen, dist, prev);
        }
    }

    hash_free(&dist);
    hash_free(&prev);
    queue_free(&pqueue);

    return pathlen;
}

int main(int argc, char** argv)
{
    position_t path[MAZE_SIZE];

    int n = find_path(MAZE, path, MAZE_SIZE, 1, 4);
    if (n > 0) {
        printf("Path found! n=%d\n", n);
        for (int i = 0; i < n; i++) {
            printf("(%d,%d) ", (int)path[i].x, (int)path[i].y);
        }
        printf("\n");
    } else {
        printf("Path NOT found!\n");
    }

    return EXIT_SUCCESS;
}

