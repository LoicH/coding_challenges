import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

example_input = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""


def read_data(s):
    indexes = []
    instructions = []
    for line in s.splitlines():
        if "," in line:
            x, y = (int(n) for n in line.split(","))
            indexes.append((x, y))
        elif line.startswith("fold along"):
            instr = line.replace("fold along ", "")
            ax, n = instr.split("=")
            instructions.append((ax, int(n)))
    indexes = np.array(indexes)
    paper = np.zeros((9999, 9999), bool)
    paper[indexes[:, 1], indexes[:, 0]] = True

    return paper, instructions


example_data = read_data(example_input)


def fold(paper: np.array, ax, n):
    height, width = paper.shape
    if ax == "x":
        folded = paper[:, :n] | paper[:, 2 * n : n : -1]
    else:
        folded = paper[:n, :] | paper[2 * n : n : -1, :]
    return folded


# Part 1
def part1(data):
    paper, instructions = data
    ax, n = instructions[0]
    paper = fold(paper, ax, n)
    return np.sum(paper > 0)


ex_paper, _ = example_data
# assert np.all(fold(ex_paper, "y", 5) == fold(ex_paper.T, "x", 5).T)
assert part1(example_data) == 17

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    paper, instructions = data
    for ax, n in instructions:
        paper = fold(paper, ax, n)
    return paper


print(part2(example_data))
result = part2(puzzle_data)
print(result)
