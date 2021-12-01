from collections import defaultdict
import numpy as np

def parse_input(path):
    with open(path) as f_in:
        data = f_in.readlines()
    return [l.rstrip() for l in data if len(l.rstrip()) > 0]

def part_one(lines, v=False):
    current_dir = "E"
    directions = defaultdict(lambda : 0)
    cardinals = ("N", "E", "S", "W")
    for l in lines:
        d, n = l[0], int(l[1:])
        if d in cardinals:
            directions[d] += n
        elif d == "F":
            directions[current_dir] += n
        elif d == "R":
            n_turns = n//90
            current_dir = cardinals[(cardinals.index(current_dir) + n_turns) % 4]
        elif d == "L":
            n_turns = n//90
            current_dir = cardinals[(cardinals.index(current_dir) - n_turns) % 4]
        if v:
            print("cur dir = {}, Directions = {}".format(current_dir, repr(directions)))
    return abs(directions["N"] - directions["S"]) + abs(directions["W"] - directions["E"])


def rotate(pos, dir, degrees):
    if dir == "R":
        r = np.array([[0,1], [-1, 0]])
    elif dir == "L":
        r = np.array([[0, -1], [1, 0]])
    return np.linalg.matrix_power(r, degrees//90).dot(pos)

def part_two(lines, v=False):
    x_boat, y_boat = (0, 0)
    x_wp, y_wp = (10, 1)
    
    for l in lines:
        d, n = l[0], int(l[1:])
        if d in ("L", "R"):
            (x_wp, y_wp) = rotate((x_wp, y_wp), d, n)
        elif d == "N":
            y_wp += n
        elif d == "S":
            y_wp -= n 
        elif d == "E":
            x_wp += n 
        elif d == "W":
            x_wp -= n
        elif d == "F":
            x_boat += n*x_wp
            y_boat += n*y_wp
        if v:
            print("{}: Boat: {}, Waypoint: {}".format(l, (x_boat, y_boat), (x_wp, y_wp)))
    return abs(x_boat)+abs(y_boat)


if __name__ == "__main__":
    lines_ex = parse_input("input_example.txt")

    assert(part_one(lines_ex, v=False) == 25), "Wrong output for part one"
    assert(part_two(lines_ex, v=False) == 286), "Wrong output for part two"

    puzzle_in = parse_input("input.txt")
    print("Part one:", part_one(puzzle_in))
    print("Part two:", part_two(puzzle_in, v=True))
