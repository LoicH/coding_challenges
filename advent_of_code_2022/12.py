from utils import *

example_input = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""


def read_data(s):
    elevations = np.array([[c for c in line] for line in s.splitlines()])
    start = np.where(elevations == "S")
    start = (start[0][0], start[1][0])
    dest = np.where(elevations == "E")
    dest = (dest[0][0], dest[1][0])
    elevations[start] = "a"
    elevations[dest] = "z"
    return elevations, start, dest


example_data = read_data(example_input)

# Part 1
def part1(data):
    return  # TODO


assert part1(example_data) == None  # TODO

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data))
