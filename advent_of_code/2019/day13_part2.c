/*
 * day13_part2.c
 * Advent of Code 2019 - Day 13
 * https://adventofcode.com/2019/day/13#part2
 *
 * Chad Gibbons
 * December 22, 2019
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
    // unlimited quarters, mofo!
    intcode->buffer[0] = 2;

    // initialize curses
    initscr();
    cbreak();
    noecho();
    clear();

    bool halted = false;
    int loops = 0;
    int ballx;
    int paddlex;
    long joystick = 0;
    useconds_t delay = 0;

    while (!halted) {
        long output[3];

        halted = run_intcode(intcode, &joystick, 1, &output[0]);
        if (halted) break;

        halted = run_intcode(intcode, NULL, 0, &output[1]);
        if (halted) break;

        halted = run_intcode(intcode, NULL, 0, &output[2]);
        if (halted) break;

        int x = output[0];
        int y = output[1];
        int tile_id = output[2];
        switch (tile_id) {
            case 3:
                paddlex = x;
                break;
            case 4:
                ballx = x;
                break;
            // fall-through otherwise
        }

        render(x, y, tile_id);

        // change the render speed after the initial screen is drawn
        if (x == -1) {
            delay = 500;
        }
        usleep(delay);

        // our game AI needs to move the paddle to where the ball is!
        int n = paddlex - ballx;
        if (n < 0) {
            joystick = 1;
        } else if (n > 0) {
            joystick = -1;
        } else {
            joystick = 0;
        }

        refresh();
        loops++;
    }

    refresh();
    getch();

    int nblocks = count_blocks();

    endwin();

    printf("# of loops executed: %d\n", loops);
    printf("# of blocks left on screen: %d\n", nblocks);
}

static void render(int x, int y, int tile_id)
{
    if (x == -1) {  // score update
        int height, width;
        getmaxyx(stdscr, height, width);
        mvwprintw(stdscr, height-1, 0, "SCORE: %d", tile_id);
    } else {
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

