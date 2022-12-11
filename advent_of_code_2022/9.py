from utils import *

example_input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""


def read_data(s):
    return [[line[0], int(line[2:])] for line in s.splitlines()]


example_data = read_data(example_input)
assert len(example_data) == 8
assert example_data[0] == ["R", 4]

DIRECTIONS = {"R": (0, 1), "L": (0, -1), "D": (1, 0), "U": (-1, 0)}


def sign(a):
    if a == 0:
        return 0
    else:
        return abs(a) // a


def distance(dx, dy):
    return max(dx, dy)


DEBUG = False


def print_grid(x_head, y_head, x_tail, y_tail, visited_positions):
    # x_tail -= x_head
    # y_tail -= y_head
    for i in range(-8, 9):
        for j in range(-8, 9):
            if i == x_head and j == y_head:
                print("H", end="")
            elif i == x_tail and j == y_tail:
                print("T", end="")
            elif (i, j) in visited_positions:
                print("⋅", end="")
            else:
                print(" ", end="")
        print()


# Part 1
def part1(data):
    x_tail, y_tail = 0, 0
    visited_positions = {(0, 0)}
    x_head, y_head = 0, 0
    # max_width = 10
    for (direction, steps) in data:
        dx, dy = DIRECTIONS[direction]
        for i in range(steps):
            if DEBUG:
                print(f"Before move {i} of {direction} {steps}")
                print_grid(x_head, y_head, x_tail, y_tail, visited_positions)
            x_head += dx
            y_head += dy
            delta_x, delta_y = x_head - x_tail, y_head - y_tail
            dist = distance(abs(delta_x), abs(delta_y))
            if dist > 1:  # let's move the tail
                x_tail += sign(delta_x)
                y_tail += sign(delta_y)
                if DEBUG:
                    print(f"After move {i} of {direction} {steps}")
                    print_grid(x_head, y_head, x_tail, y_tail, visited_positions)
                visited_positions.add((x_tail, y_tail))
    return len(visited_positions)


assert part1(example_data) == 13

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
