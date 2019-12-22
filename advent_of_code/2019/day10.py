#!/usr/bin/env python3
#
# day10.py
# Advent of code 2019 - Day 10
# https://adventofcode.com/2019/day/10
#
# Chad Gibbons
# December 20, 2019
#

import math
import sys


def read_map(filename):
    # reads a map from an input file - assuming one row per line of text
    map = []
    with open(filename) as fp:
        line = fp.readline()
        while line:
            map.append(line.rstrip())  # remove trailing newlines
            line = fp.readline()
    return map


def find_asteroids(map):
    # look at the entire map and extract a list of asteroids to simply
    # further processing
    n = len(map[0])
    asteroids = list()
    for x in range(0, n):
        for y in range(0, n):
            if map[y][x] == '#':
                asteroids.append((x,y))
    return asteroids


def find_max_visible_asteroids(map):
    # compute the slope between every asteroid and every other asteroid - any
    # asteroids that share the same slope between another asteroid will only be
    # counted as visible once, since the other asteroid obscures it
    best = 0
    for a in asteroids:
        slopes = set()
        for b in asteroids:
            if a == b: continue  # skip ourselves

            # slope here is defined as the X and Y distance between each other,
            # and then that fraction reduced so that the entire map is
            # normalized
            slope = (b[1]-a[1], b[0]-a[0])
            gcd = math.gcd(slope[0], slope[1])
            slope = (slope[0]/gcd,slope[1]/gcd)

            slopes.add(slope)

        # the total number of unique slopes is how many asteroids this asteroid
        # can see; keep track of the highest number so far
        if len(slopes) > best:
            best = len(slopes)
            print("best asteroid is %r with %d asteroids visible" % (a, best))

    return best

if __name__ == '__main__':
    map = read_map(sys.argv[1])
    asteroids = find_asteroids(map)
    best = find_max_visible_asteroids(map)
    print(best)

