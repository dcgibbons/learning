#!/usr/bin/env python3
#
# day16_part2.py
# Advent of Code 2019 - Day 16, Part 2
# https://adventofcode.com/2019/day/16#part2
# 
# Chad Gibbons
# December 29, 2019
#
# Another solution implemented entirely from reddit hints... this one required
# looking closely at the numerical patterns in the data and pattern, and using
# that to calculate only the subset of data that needed to be calculated.
#

import sys


def read_input(filename):
    data = []
    with open(filename) as fp:
        line = fp.readline()
        while line:
            line = line.rstrip()
            for n in line:
                data.append(ord(n) - ord('0'))
            line = fp.readline()
    return data


def msg_offset(data):
    raw_offset = data[0:7]
    offset = ''.join(map(str, raw_offset))
    return int(offset)


if __name__ == '__main__':
    data = read_input(sys.argv[1])
    offset = msg_offset(data)
    n = len(data)
    print("offset=%r len(data)=%d" % (offset, n))

    # duplicate the data 10,000 times
    for i in range(0, 9999):
        for j in range(0, n):
            data.append(data[j])
    n = len(data)
    print("data * 10000, len(data)=%d offset-len=%d" % (n, n-offset))

    # simulate 100 cycles of FFT by doing just the calculation that matters
    for i in range(0, 100):
        for j in range(n-2, offset-1, -1):
            data[j] = (data[j] + data[j+1]) % 10

    new_signal = data[offset:offset+8]
    print("final signal:", new_signal)

