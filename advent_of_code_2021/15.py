import itertools
import heapq
import math
import re
from collections import Counter, defaultdict

import aocd
import matplotlib.animation
import matplotlib.pylab as plt
import numpy as np
import seaborn as sns
from tqdm import tqdm, trange

example_input = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"""


def read_data(s):
    return [[int(n) for n in line] for line in s.splitlines()]


example_data = read_data(example_input)
assert len(example_data) == 10
assert example_data[0][0] == 1


def neighbour(x, y, h, w):
    return {
        (x + dx, y + dy)
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        if (dx != 0 or dy != 0) and 0 <= x + dx < h and 0 <= y + dy < w
    }


# Part 1
def part1(data):
    # Re-write the A* algorithm
    height = len(data)
    width = len(data[0])
    src = (0, 0)
    dest = (height - 1, width - 1)
    to_visit = [(0, src)]
    min_dist = defaultdict(lambda: math.inf, {src: 0})
    visited = set()

    while len(to_visit) > 0:

        dist, current = heapq.heappop(to_visit)
        x_cur, y_cur = current
        if current == dest:
            return dist
        visited.add(current)
        for x_neighb, y_neighb in neighbour(x_cur, y_cur, height, width) - visited:
            risk = dist + data[x_neighb][y_neighb]
            if risk < min_dist[(x_neighb, y_neighb)]:
                min_dist[(x_neighb, y_neighb)] = risk
                heapq.heappush(to_visit, [risk, (x_neighb, y_neighb)])


assert part1(example_data) == 40

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    for i, line in enumerate(data):
        data[i] = [((n + j - 1) % 9) + 1 for j in range(0, 5) for n in line]
    new_data = []
    for i in range(5):
        for line in data:
            new_data.append([((n + i - 1) % 9) + 1 for n in line])
    return part1(new_data)


assert part2(example_data) == 315

aocd.submit(part2(puzzle_data))
