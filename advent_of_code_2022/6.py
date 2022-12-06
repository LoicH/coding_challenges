from utils import * 

example_input = """mjqjpqmgbljsphdztnvjfqwrcgsmlb"""


def read_data(s):
    return s


example_data = read_data(example_input)

# Part 1
def part1(data):
    for i in range(len(data)):
        if len(set(data[i:i+4])) == 4:
            break
    return i+4


assert part1(example_data) == 7

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    L = 14
    for i in tqdm(range(len(data))):
        if len(set(data[i:i+L])) == L:
            break
    return i+L


assert part2(example_data) == 19

aocd.submit(part2(puzzle_data))
