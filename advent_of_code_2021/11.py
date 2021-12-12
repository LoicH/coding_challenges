import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

example_input = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""


def read_data(s):
    return np.array([[int(n) for n in l] for l in s.splitlines()])


example_data = read_data(example_input)
assert len(example_data) == 10
assert example_data[0][0] == 5


def neighbours(i, j, m):
    return set(
        (i + di, j + dj)
        for di, dj in itertools.product([-1, 0, 1], repeat=2)
        if 0 <= i + di <= 9 and 0 <= j + dj <= 9 and (di != 0 or dj != 0)
    )


def step(m):
    new_m = m + 1
    to_flash = set(zip(*np.where(new_m > 9)))
    flashed = set()
    while len(to_flash) > 0:
        (i, j) = to_flash.pop()
        neighb = neighbours(i, j, new_m) - flashed
        new_m[list(zip(*neighb))] += 1
        will_flash = {(x, y) for (x, y) in neighb if new_m[x, y] > 9} - flashed
        to_flash = to_flash.union(will_flash)
        flashed.add((i, j))
    new_m[list(zip(*flashed))] = 0
    return len(flashed), new_m


# Part 1
def part1(data, n_steps=100):
    total = 0
    for _ in range(n_steps):
        n, data = step(data)
        total += n
    return total


assert part1(example_data, n_steps=10) == 204
assert part1(example_data, n_steps=100) == 1656

puzzle_input = aocd.get_data(day=11)

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(puzzle_data), day=11)

# Part 2
def part2(data):
    i = 0
    while True:
        n, data = step(data)
        i += 1
        if n == data.size:
            return i


assert part2(example_data) == 195

aocd.submit(part2(puzzle_data))
