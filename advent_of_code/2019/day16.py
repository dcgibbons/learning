#!/usr/bin/env python3
#
# day16.py
# Advent of Code 2019 - Day 16
# https://adventofcode.com/2019/day/16
# 
# Chad Gibbons
# December 29, 2019
#

import sys

class Pattern(object):
    def __init__(self, repeat=1, base_pattern=[0, 1, 0, -1]):
        self._pattern = base_pattern
        self._pattern_len = len(base_pattern)
        self._pattern_repeat = repeat
        self._pattern_current_repeat = 1
        self._pattern_index = 0

    def __iter__(self):
        return self

    def __next__(self):
        self._pattern_current_repeat += 1
        if self._pattern_current_repeat > self._pattern_repeat:
            self._pattern_current_repeat = 1
            self._pattern_index = (self._pattern_index + 1) % self._pattern_len

        value = self._pattern[self._pattern_index]
        return value


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


def fft(base_signal):
    new_signal = []
    for i in range(0, n):
        pattern = Pattern(repeat=i+1)
        a = 0
        for j in range(0, n):
            p = next(pattern)
            print("%d*%d +" % (base_signal[j], p), end=" ")
            a += base_signal[j] * p
        a = abs(a) % 10
        print("=", a)
        new_signal.append(a)
    return new_signal


if __name__ == '__main__':
    data = read_input(sys.argv[1])
    n = len(data)

    new_signal = data
    for phase in range(0, 100):
        new_signal = fft(new_signal)

    print(new_signal[0:8])
        
