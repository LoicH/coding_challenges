import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """2199943210
3987894921
9856789892
8767896789
9899965678"""


def read_data(s):
    return [[int(c) for c in line] for line in s.splitlines()]


test_nb = read_data(test_input)
assert len(test_nb) == 5
assert test_nb[0][0] == 2

# Part 1
def part1(data):
    # Padding
    height, width = len(data), len(data[0])
    data = [[9] * width] + data + [[9] * width]
    data = [[9] + line + [9] for line in data]

    # Summing risk levels
    total = 0
    directions = [(0, -1), (0, 1), (-1, 0), (1, 0)]
    for i in range(1, height + 1):
        for j in range(1, width + 1):
            if data[i][j] < min(data[i + di][j + dj] for di, dj in directions):
                total += data[i][j] + 1
    return total


assert part1(test_nb) == 15

raw_data = aocd.get_data()

input_data = read_data(raw_data)
# aocd.submit(part1(input_data))


def assign_basin(data, basins, basins_map, i, j):
    # If there isn't already a basin at (i,j), we need to see if there's already
    # a basin at (i, j+1)
    if basins_map[i][j] is None:
        if data[i][j] != 9:
            if j < len(data[i]) - 1 and basins_map[i][j + 1] is not None:
                # Let's assign this point to the basin that's already on the right
                new_basin = basins_map[i][j + 1]
            else:
                # Let's assign to a new basin
                new_basin = max(basins.keys(), default=0) + 1
            basins_map[i][j] = new_basin
            basins[new_basin].add((i, j))
    # If this basin was already assigned (by scanning the data point on the left or above)
    # then we need to check if there is also another basin on our right, and merge them if there is
    elif (
        j < len(data[i]) - 1
        and basins_map[i][j + 1] is not None
        and basins_map[i][j + 1] != basins_map[i][j]
    ):
        current_basin = basins_map[i][j]
        right_basin = basins_map[i][j + 1]
        for (x, y) in basins[right_basin]:
            basins_map[x][y] = current_basin
            basins[current_basin].add((x, y))
        basins.pop(right_basin)
    return basins, basins_map


def propagate_basin(data, basins, basins_map, i, j):
    cur_basin = basins_map[i][j]
    if cur_basin is None:
        return basins, basins_map
    # Let's propagate on our right
    if j < len(basins_map[i]) - 1 and data[i][j + 1] < 9:
        basins_map[i][j + 1] = cur_basin
        basins[cur_basin].add((i, j + 1))
    # Let's propagate below
    if i < len(basins_map) - 1 and data[i + 1][j] < 9:
        basins_map[i + 1][j] = cur_basin
        basins[cur_basin].add((i + 1, j))
    return basins, basins_map


# Part 2
def part2(data):
    basins = defaultdict(set)
    basins_map = [[None] * len(data[0]) for line in data]
    for i, line in enumerate(basins_map):
        for j, basin in enumerate(line):
            basins, basins_map = assign_basin(data, basins, basins_map, i, j)
            basins, basins_map = propagate_basin(data, basins, basins_map, i, j)
    big_bassins_sizes = sorted([len(s) for s in basins.values()])[-3:]
    return big_bassins_sizes[0] * big_bassins_sizes[1] * big_bassins_sizes[2]


assert part2(test_nb) == 1134

aocd.submit(part2(input_data))
