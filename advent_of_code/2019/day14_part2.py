#!/usr/bin/env python3
#
# day14_part2.py
#
# Advent of Code 2019 - Day 14
# https://adventofcode.com/2019/day/14#part2
#
# Chad Gibbons
# December 25, 2019
#

import collections
import copy
import math
import queue
import sys


class Chemical(object):
    def __init__(self, name, qty):
        self.name = name
        self.qty = qty
    def __repr__(self):
        return f"Chemical({self.name},{self.qty})"
    def __hash__(self):
        return hash(self.name)
    def __eq__(self, other):
        # allow quality based on Chemical objects and strings
        if self.__class__ == other.__class__ and self.name == other.name:
            return True
        else:
            return self.name == other


def read_reactions(filename):
    reactions = dict()

    with open(filename) as fp:
        line = fp.readline()
        while line:
            line = line.rstrip()

            # split line into reactants and the result
            r = line.split("=>")
            reactants = r[0].strip()
            result = r[1].strip()

            res = result.split(' ')
            qty = int(res[0])
            name = res[1].strip()
            result = Chemical(name, qty)
            reactions[result] = list()

            for r in reactants.split(','):
                u = r.strip().split(' ')
                qty = int(u[0])
                name = u[1].strip()
                reactions[result].append(Chemical(name, qty))

            line = fp.readline()

    return reactions

def make_fuel(amount, supply, reactions):
    total_fuel = 0
    orders = queue.Queue()
    orders.put({"ingredient":"FUEL", "amount": amount})
    ore_needed = 0
    last_ore_used = 0

    while not orders.empty():
        order = orders.get()

        chemical = order["ingredient"]
        qty = order["amount"]

        if chemical == "ORE":
            if qty <= supply["ORE"]:
                ore_needed += qty
                supply["ORE"] -= qty
            else:
                print("exhausted ore")
                return (0, 0)
        elif qty <= supply[chemical]:
            supply[chemical] -= qty
        else:
            amount_needed = qty - supply[chemical]
            for k,v in reactions.items():
                if k.name == chemical:
                    recipe = k
                    break
            batches = math.ceil(amount_needed / recipe.qty)
            for c in reactions[chemical]:
                orders.put({"ingredient": c.name, "amount": c.qty * batches})
            leftover_amount = batches * recipe.qty - amount_needed
            supply[chemical] = leftover_amount

    return (amount, ore_needed)

if __name__ == '__main__':
    reactions = read_reactions(sys.argv[1])

    fuel = None
    for k in reactions.keys():
        if k == "FUEL":
            fuel = k
            break
    print("Fuel:", fuel)

    available_ore = 1000000000000
    supply = collections.defaultdict(int)
    supply["ORE"] = available_ore

    qty = fuel.qty * 100000
    total_fuel = 0
    total_ore = 0
    while qty > 0:
        last_supply = copy.deepcopy(supply)
        (x, y) = make_fuel(qty, supply, reactions)
        if x == 0:
            supply = copy.deepcopy(last_supply)
            qty = int(qty / 2)
        else:
            total_fuel += x
            total_ore += y
        print("Total Fuel:%d Total ORE required:%d asking for:%d" % (total_fuel, total_ore, qty))

    print("Total Fuel:%d Total ORE required:%d asking for:%d" % (total_fuel, total_ore, qty))
    print("Remaining supplies:", last_supply)

