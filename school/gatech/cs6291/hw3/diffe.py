#!/usr/bin/env python
#
# Differential Encoding of Registers
#
# CS6291 - Embedded Systems Optimizatons
# Georgia Institute of Technology
# Fall, 2018
#
# David Chadwick Gibbons
#

import sys
from itertools import permutations

regs = { 'x':2, 'z':5, 'y':1, 'u':6, 'w':3, 'v':4 }
vregs = regs.keys()
diffw = 2
max_diff = pow(2,diffw)
regn = 6

adj_list = [
    ('y','x',1),
    ('y','w',1),
    ('y','z',1),
    ('x','y',2),
    ('x','w',1),
    ('x','v',1),
    ('v','u',1),
    ('w','u',1),
    ('w','z',1),
    ('z','x',2)
]

def calc_cost(r):
    cost = 0
    for curr_reg,next_reg,edge_cost in adj_list:
        diff = (r[curr_reg] - r[next_reg]) % regn
        if diff >= max_diff:
            cost += edge_cost
    return cost


def find_lowest_cost(r, swaps):
    min_cost = sys.maxint
    min_regs = None
    min_k, min_p = None, None
    for k in range(0, len(vregs)):
        for p in range(k, len(vregs)):
            if p is k: continue
            vreg_k = vregs[k]
            vreg_p = vregs[p]
            skip = False
            for a,b in swaps:
                if (a,b) == (vreg_k,vreg_p):
                    print("skipping %r,%r as it was already swapped" %
                            (vreg_k,vreg_p))
                    skip = True
                    break
            if not skip:
                new_regs = r.copy()
                new_regs[vreg_k], new_regs[vreg_p] = r[vreg_p], r[vreg_k]
                cost = calc_cost(new_regs)
                print("Swap %r and %r - Mapping: %r - Cost: %r" % (vreg_k,vreg_p,new_regs,cost))
                if cost < min_cost:
                    min_k, min_p  = vreg_k, vreg_p
                    min_cost = cost
                    min_regs = new_regs.copy()

    if min_regs is None:
        return (None,0,None,None)
    else:
        print("Min. Cost - Swap %r and %r - Mapping: %r - Cost: %r" %
                (min_k,min_p,min_regs,min_cost))
        return (min_regs,min_cost,min_k,min_p)

my_regs = regs.copy()
swaps = []
i = 0
while True: # i < 2:
    print("**** Iteration #%d **** " % (i))
    my_regs, new_cost, k, p = find_lowest_cost(my_regs, swaps)
    if my_regs is None: break
    swaps.append((k,p))
    i += 1
    print

