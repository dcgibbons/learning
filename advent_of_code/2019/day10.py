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

if __name__ == '__main__':
    map = []
    with open(sys.argv[1]) as fp:
        line = fp.readline()
        while line:
            map.append(line.rstrip())
            line = fp.readline()
    n = len(map[0])

    asteroids = list()
    for x in range(0, n):
        for y in range(0, n):
            if map[y][x] == '#':
                asteroids.append((x,y))

    best = 0
    for a in asteroids:
        slopes = set()
        for b in asteroids:
            if a == b: continue
            slope = (b[1]-a[1], b[0]-a[0])
            gcd = math.gcd(slope[0], slope[1])
            slope = (slope[0]/gcd,slope[1]/gcd)
            slopes.add(slope)
        if len(slopes) > best:
            best = len(slopes)
    print(best)

