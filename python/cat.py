#!/usr/bin/python

import sys
f = open(sys.argv[0], 'r');
line = 0
for s in f.readline():
    line = line+1
    print "line #%04d: %s\n" % (line, s)
