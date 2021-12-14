import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

example_input = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""


def read_data(s):
    template, rules = s.split("\n\n")
    rules = [rule.split(" -> ") for rule in rules.splitlines()]
    rules = {a: b for a, b in rules}
    return template, rules


example_data = read_data(example_input)


def step(couples, rules):
    new_couples = defaultdict(lambda: 0)
    for (a, b), n in couples.items():
        c = rules[a + b]
        new_couples[a + c] += n
        new_couples[c + b] += n
    return new_couples


# Part 1
def part1(poly, rules, n_steps=10):
    couples = defaultdict(lambda: 0)
    for i in range(len(poly) - 1):
        couples[poly[i : i + 2]] += 1

    for i in trange(n_steps):
        couples = step(couples, rules)

    counts = defaultdict(lambda: 0)
    for i, ((a, b), n) in enumerate(couples.items()):
        if i == 0:
            counts[a] += n
        counts[b] += n

    return max(counts.values()) - min(counts.values())


assert part1(*example_data) == 1588  # TODO

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(*puzzle_data))

# Part 2
def part2(data):
    return part1(*data, n_steps=40)


assert part2(example_data) == 2188189693529

aocd.submit(part2(puzzle_data))
