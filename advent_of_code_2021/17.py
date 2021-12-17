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
    # For our probe to have a chance to ever reach the target zone,
    # The horizontal speed x must cannot be too small, because
    # x + (x-1) + ... + 1 >= x_min
    # i.e. sum of k from 1 to x >= x_min
    # i.e. x * (x+1) / 2 >= x_min
    # therefore x^2 + x - 2 * x_min >= 0
    # therefore x >= (sqrt(8*xmin + 1) - 1) / 2
    x_lower = math.ceil((math.sqrt(1 + 8 * x_min) - 1) / 2)
    # And if x > x_max, the first step will overshoot the target zone
    x_upper = x_max
    # If y_max<0, then inital vertical velocity is must be lower than |y_min|,
    # because if the initial dy is |y_min| + 1 or more,
    # at step number dy we reach the maximum height
    # and then at step 2*dy we reach y=0 again, with a vertical velocity of -dy
    # therefore at step 2*dy + 1 we will reach y = -dy - 1, which is greater than y_min
    # So we jumped over the target zone
    y_upper = -y_min
    # And if dy < y_min, the first step will miss the target zone
    y_lower = y_min
    n_values = 0
    for x, y in tqdm(
        itertools.product(range(x_lower, x_upper + 1), range(y_lower, y_upper + 1))
    ):
        if reach_target(x, y, x_min, x_max, y_min, y_max):
            n_values += 1
    return n_values


assert part2(*example_data) == 112

aocd.submit(part2(*puzzle_data))
