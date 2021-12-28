from utils import *


with open("example_input_20.txt", "r") as fp:
    example_input = fp.read()


def read_data(s):
    raw_rules, raw_start = s.split("\n\n")
    rules = raw_rules.replace("\n", "")
    rules = [int(c == "#") for c in rules]
    start = [[int(c == "#") for c in line] for line in raw_start.split()]
    return rules, start


example_data = read_data(example_input)


def value(grid, i, j):
    if i == 0 or i == len(grid) - 1 or j == 0 or j == len(grid[0]) - 1:
        val = grid[i][j] * 511
    else:
        val = int(
            "".join(
                str(grid[i + di][j + dj])
                for di, dj in itertools.product([-1, 0, 1], repeat=2)
            ),
            2,
        )
    return val


def step(rules, grid):
    # Padding with the symbol on the outside
    outside = grid[0][0]
    grid = [[outside] * len(grid[0])] + grid + [[outside] * len(grid[0])]
    new_grid = [[outside] + line + [outside] for line in grid]
    height, width = len(new_grid), len(new_grid[0])

    values = [[value(new_grid, i, j) for j in range(width)] for i in range(height)]
    new_pixels = [[rules[values[i][j]] for j in range(width)] for i in range(height)]
    return new_pixels


def print_grid(grid):
    for line in grid:
        print("".join(".#"[pixel] for pixel in line))


# Part 1
def part1(rules, grid, n_iter=2):
    new_grid = [[0] * len(grid[0])] + grid + [[0] * len(grid[0])]
    new_grid = [[0] + line + [0] for line in new_grid]
    # print("Starting grid:")
    # print_grid(new_grid)
    for i in trange(n_iter):
        new_grid = step(rules, new_grid)
        # print("New grid:")
        # print_grid(new_grid)
    return sum(sum(line) for line in new_grid)


assert part1(*example_data) == 35

puzzle_input = aocd.get_data(day=20)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(*puzzle_data), day=20)
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(rules, grid):
    return part1(rules, grid, n_iter=50)


assert part2(*example_data) == 3351

aocd.submit(part2(*puzzle_data), day=20)
