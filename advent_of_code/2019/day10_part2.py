#!/usr/local/opt/python@3.8/bin/python3
#
# day10_part2.py
# Advent of code 2019 - Day 10
# https://adventofcode.com/2019/day/10
#
# Chad Gibbons
# December 21, 2019
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
    width = len(map[0])
    height = len(map)
    asteroids = list()
    for x in range(0, width):
        for y in range(0, height):
            if map[y][x] == '#':
                asteroids.append((x,y))
    return asteroids


def find_angles_to_all_asteroids(asteroids, origin):
    # calculate and store the angle between our station and every other asteroid 
    x0,y0 = origin
    angles = dict()
    for (x1,y1) in asteroids:
        if (x1,y1) == origin: continue  # skip ourself!
        dist = math.dist((x0,y0), (x1,y1))
        angle = math.atan2(y0-y1, x0-x1)
        if angle not in angles:
            angles[angle] = [(dist,(x1,y1))]
        else:
            angles[angle].append((dist,(x1,y1)))
    return angles


def asteroids_left(angles):
    nasteroids = 0
    for (a,b) in angles:
        nasteroids += len(b)
    return nasteroids


def destroy_asteroids(angles):
    # sort all the angles so it can be processed iteratively
    sorted_list = []
    for key in sorted(angles):
        sorted_list.append((key, sorted(angles[key])))

    # find the 90-degree starting point
    i = 0
    for a in sorted_list:
        if a[0] == math.pi / 2: break
        i += 1

    # cycle through the angles clockwise, removing one asteroid at a time
    # from each angle
    destroyed_asteroids = []
    total_angles = len(sorted_list)
    while asteroids_left(sorted_list) > 0:
        (angle,a) = sorted_list[i]
        if len(a) > 0:
            b = a.pop(0)
            destroyed_asteroids.append(b[1])
        i += 1
        if i >= total_angles: i = 0
    return destroyed_asteroids


if __name__ == '__main__':
    map = read_map(sys.argv[1])
    width = len(map[0])
    height = len(map)
    best = (37, 25)  # TODO make this an argument

    asteroids = find_asteroids(map)
    angles = find_angles_to_all_asteroids(asteroids, best)
    destroyed_asteroids = destroy_asteroids(angles)

    a = destroyed_asteroids[200-1]
    print("destroyed asteroid #200:", a)
    print("\tcode:", a[0] * 100 + a[1])

