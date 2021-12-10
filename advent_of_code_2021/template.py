import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

example_input = """bla"""


def read_data(s):
    return [int(n) for n in s.split(",")]


example_data = read_data(example_input)
assert len(example_data) == None  # TODO
assert example_data[0] == None  # TODO

# Part 1
def part1(data):
    return  # TODO


assert part1(example_data) == None  # TODO

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data))
