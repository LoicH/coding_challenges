import re
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """3,4,3,1,2"""

test_data = test_input.splitlines()


def read_data(lines):

    return [int(n) for n in lines[0].split(",")]


test_nb = read_data(test_data)
assert len(test_nb) == 5
assert test_nb[0] == 3

# Part 1
def part1(data, n_days):
    fishes = dict(Counter(data))
    for i in range(n_days):
        new_fishes = {k - 1: v for k, v in fishes.items() if k >= 0}
        fishes_born = new_fishes.get(-1)
        if fishes_born:
            if 6 in new_fishes:
                new_fishes[6] += fishes_born
            else:
                new_fishes[6] = fishes_born
            new_fishes[8] = fishes_born
            new_fishes[-1] = 0
        fishes = new_fishes
    return sum(fishes.values())


assert part1(test_nb, 18) == 26
assert part1(test_nb, 80) == 5934

with open("input_6.txt", "r") as fp:
    data = [line for line in fp.readlines()]

input_data = read_data(data)
print("1st part:", part1(input_data, 80))

# Part 2
def part2(data):
    return part1(data, 256)


assert part2(test_nb) == 26984457539


print("2nd part:", part2(input_data))
