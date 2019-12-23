/*
 * day13.c
 * Advent of Code 2019 - Day 13
 * https://adventofcode.com/2019/day/13
 *
 * Chad Gibbons
 * December 22, 2019
 */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ncurses.h>

#include "intcode.h"

/* forward reference */
static void play_arcade(intcode_t intcode);
static void render(int x, int y, int tile_id);
static int count_blocks();

int main(int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s [intcode program]\n", argv[0]);
        exit(EXIT_FAILURE);
    } else {
        const char* input_filename = argv[1];
        intcode_t intcode = read_intcode(input_filename);
        if (intcode == NULL) {
            perror("unable to read intcode");
            exit(errno);
        } else {
            play_arcade(intcode);
            free_intcode(intcode);
        }
    }
    return EXIT_SUCCESS;
}

static void play_arcade(intcode_t intcode)
{
    // initialize curses
    initscr();
    cbreak();
    noecho();
    clear();

    bool halted = false;
    int loops = 0;
    while (!halted) {
        long output[3];

        halted = run_intcode(intcode, NULL, 0, &output[0]);
        if (halted) break;

        halted = run_intcode(intcode, NULL, 0, &output[1]);
        if (halted) break;

        halted = run_intcode(intcode, NULL, 0, &output[2]);
        if (halted) break;

        int x = output[0];
        int y = output[1];
        int tile_id = output[2];
        render(x, y, tile_id);

        loops++;
    }

    refresh();
    getch();  // wait for user to press key before exiting
    int nblocks = count_blocks();
    endwin();

    printf("# of loops executed: %d\n", loops);
    printf("# of blocks left on screen: %d\n", nblocks);
}

static void render(int x, int y, int tile_id)
{
    char ch = ' ';
    switch (tile_id) {
        case 0:
            ch = ' ';
            break;
        case 1:
            ch = '#';
            break;
        case 2:
            ch = 'B';
            break;
        case 3:
            ch = '_';
            break;
        case 4:
            ch = 'o';
            break;
        default:
            abort();
    }
    mvaddch(y, x, ch);
}

static int count_blocks()
{
    int height, width;

    getmaxyx(stdscr, height, width);

    int nblocks = 0;
    chtype buffer[BUFSIZ];
    for (int y = 0; y < height; y++) {
        int n = mvwinchnstr(stdscr, y, 0, buffer, sizeof(buffer));
        for (int i = 0; i < n; i++) {
            if (buffer[i] == 'B') nblocks++;
        }
    }

    return nblocks;
}

