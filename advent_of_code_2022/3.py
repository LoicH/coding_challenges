import string

from utils import * 

example_input = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""


def read_data(s):
    return s.splitlines()


example_data = read_data(example_input)
assert len(example_data) == 6
assert example_data[0] == "vJrwpWtwJgWrhcsFMMfFFhFp"

LETTERS_VALUES = {c:i+1 for i,c in enumerate(string.ascii_letters)}

# Part 1
def part1(data):
    score = 0
    for line in data:
        half = len(line)//2
        left, right = line[:half], line[half:]
        common = set(left).intersection(set(right))
        score += sum(LETTERS_VALUES[c] for c in common)
    return score

assert part1(example_data) == 157

puzzle_input = aocd.get_data(day=3)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    # Create groups of 3
    i = iter(data)
    groups = list(zip(i, i, i))
    score = 0
    for g in groups:
        common = set.intersection(*[set(e) for e in g]).pop()
        score += LETTERS_VALUES[common]
    return score

assert part2(example_data) == 70

aocd.submit(part2(puzzle_data))
