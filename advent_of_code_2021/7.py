import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """16,1,2,0,4,2,7,1,2,14"""
test_data = test_input


def read_data(s):
    return [int(n) for n in s.split(",")]


test_nb = read_data(test_data)
assert len(test_nb) == 10
assert test_nb[0] == 16

# Part 1
def part1(data):
    target = int(np.median(data))
    return sum(abs(n - target) for n in data)


assert part1(test_nb) == 37

raw_data = aocd.get_data()

input_data = read_data(raw_data)
# aocd.submit(part1(input_data))

# Part 2
def part2(data):
    min_total = None
    for target in range(1, max(data)):
        total = 0
        for pos in data:
            d = abs(pos - target) + 1
            total += d * (d - 1) / 2
        print("{} : {:_}".format(target, total))
        # the total should be decreasing.
        # If we found the minimum, let's stop
        if not min_total or total < min_total:
            min_total = total
        else:
            break
    return int(min_total)


assert part2(test_nb) == 168

# aocd.submit(
print(part2(input_data))  # )
#
