import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """bla"""


def read_data(s):
    return [int(n) for n in s.split(",")]


test_nb = read_data(test_input)
assert len(test_nb) == None  # TODO
assert test_nb[0] == None  # TODO

# Part 1
def part1(data):
    return  # TODO


assert part1(test_nb) == None  # TODO

raw_data = aocd.get_data()

input_data = read_data(raw_data)
aocd.submit(part1(input_data))

# Part 2
def part2(data):
    return None  # TODO


assert part2(test_nb) == None  # TODO

aocd.submit(part2(input_data))
