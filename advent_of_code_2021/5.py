import re
import itertools
from collections import defaultdict

test_input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

test_data = test_input.splitlines()


def read_data(lines):
    numbers = []
    for line in lines:
        split = re.split(r"\b", line.rstrip())
        numbers.append([int(n) for n in split if n not in ("", ",", " -> ")])
    return numbers


test_nb = read_data(test_data)
assert len(test_nb) == 10
assert len(test_nb[0]) == 4

# Part 1
def part1(data):
    board = defaultdict(int)
    for x1, y1, x2, y2 in data:
        if x1 != x2 and y1 != y2:
            continue
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                board[(x, y)] += 1
    return len([v for k, v in board.items() if v > 1])


assert part1(test_nb) == 5

with open("input_5.txt", "r") as fp:
    data = [line for line in fp.readlines()]

input_data = read_data(data)
print("1st part:", part1(input_data))

# Part 2
def part2(data):
    board = defaultdict(int)
    for x1, y1, x2, y2 in data:
        if x1 != x2 and y1 != y2:
            dx = 1 if x1 < x2 else -1
            dy = 1 if y1 < y2 else -1
            for x, y in zip(range(x1, x2 + dx, dx), range(y1, y2 + dy, dy)):
                board[(x, y)] += 1
        else:
            for x in range(min(x1, x2), max(x1, x2) + 1):
                for y in range(min(y1, y2), max(y1, y2) + 1):
                    board[(x, y)] += 1

    return len([v for k, v in board.items() if v > 1])


assert part2(test_nb) == 12


print("2nd part:", part2(input_data))
