import itertools
import math
import re
from collections import Counter, defaultdict

import aocd
import numpy as np
from tqdm import tqdm, trange

example_input = """target area: x=20..30, y=-10..-5"""


def read_data(s):
    pattern = r"target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)"
    matches = re.search(pattern, s)
    x_min, x_max = int(matches.group(1)), int(matches.group(2))
    y_min, y_max = int(matches.group(3)), int(matches.group(4))
    return x_min, x_max, y_min, y_max


example_data = read_data(example_input)

# Part 1
def part1(data):
    # Easy math (kidding)
    _, _, y_min, _ = data
    return y_min * (y_min + 1) / 2


assert part1(example_data) == 45

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
aocd.submit(part1(puzzle_data))


def reach_target(x, y, x_min, x_max, y_min, y_max, max_steps=1000):
    dx, dy = x, y
    reach = False
    for _ in range(max_steps):
        if x_min <= x <= x_max and y_min <= y <= y_max:
            reach = True
            break
        elif x > x_max or y < y_min:
            break
        if dx != 0:
            dx -= dx / abs(dx)  # dx-- if dx>0 else dx++
        dy -= 1
        x += dx
        y += dy
    return reach


# Part 2
def part2(x_min, x_max, y_min, y_max):
    x_lower = math.ceil((math.sqrt(1 + 8 * x_min) - 1) / 2)
    x_upper = x_max
    y_lower = y_min
    y_upper = -y_min  # Works only if the target area is below the submarine,
    # i.e. if y_min < y_max < 0
    n_values = 0
    for x, y in itertools.product(
        range(x_lower, x_upper + 1), range(y_lower, y_upper + 1)
    ):
        if reach_target(x, y, x_min, x_max, y_min, y_max):
            n_values += 1
    return n_values


assert part2(*example_data) == 112

aocd.submit(part2(*puzzle_data))
