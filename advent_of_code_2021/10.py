import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""


def read_data(s: str):
    return s.splitlines()


test_data = read_data(test_input)
assert len(test_data) == 10
assert test_data[0][0] == "["

CLOSE_OPEN = {b: a for a, b in ("{}", "[]", "()", "<>")}
VALUES = {"}": 1197, "]": 57, ")": 3, ">": 25137}

# Part 1
def part1(data):
    total = 0
    for line in data:
        stack = []
        for c in line:
            # If we find a closing char:
            if c in CLOSE_OPEN:
                if len(stack) > 0 and stack[-1] == CLOSE_OPEN[c]:
                    stack.pop()
                else:
                    total += VALUES[c]
                    break
            else:
                stack.append(c)
    return total


assert part1(test_data) == 26397

puzzle_input = aocd.get_data()

input_data = read_data(puzzle_input)
# aocd.submit(part1(input_data))


def complete(line):
    values = {"}": "3", "]": "2", ")": "1", ">": "4"}
    open_close = {v: k for k, v in CLOSE_OPEN.items()}  # {->}
    stack = []
    for c in line:
        # If we find a closing char:
        if c in CLOSE_OPEN:
            if len(stack) > 0 and stack[-1] == CLOSE_OPEN[c]:
                stack.pop()
            else:
                return None
        else:
            stack.append(c)
    completion = "".join(values[open_close[c]] for c in stack[::-1])
    return int(completion, 5)


# Part 2
def part2(data):
    scores = [complete(line) for line in data]
    sorted_scores = sorted([s for s in scores if s is not None])
    return sorted_scores[int((len(sorted_scores) - 1) / 2)]


assert part2(test_data) == 288957

aocd.submit(part2(input_data))
