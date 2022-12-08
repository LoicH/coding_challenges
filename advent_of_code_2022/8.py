from utils import * 

example_input = """30373
25512
65332
33549
35390
"""


def read_data(s):
    return [[int(c) for c in line] for line in s.splitlines()]


example_data = read_data(example_input)
assert len(example_data) == 5
assert example_data[0] == [3,0,3,7,3]

def visible(l):
    max = None
    visible = set()
    for (i, n) in enumerate(l):
        if max is None or max < n:
            visible.add(i)
            max = n
    return visible

# Part 1
def part1(data):
    idx_visible = set()
    height, width = len(data), len(data[0])
    for (i, line) in enumerate(data):
        visible_left = visible(line)
        idx_visible.update({(i, j) for j in visible_left})
        visible_right = visible(line[::-1])
        idx_visible.update({(i, width - j - 1) for j in visible_right})
    for j in range(len(data[0])):
        col = [l[j] for l in data]
        visible_down = visible(col) # visible when looking down
        idx_visible.update({(i, j) for i in visible_down})
        visible_up = visible(col[::-1])
        idx_visible.update({(height-i-1, j) for i in visible_up})
    return len(idx_visible)


assert part1(example_data) == 21

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

def scenic_score(data, i, j):
    dist_right, dist_left = 0, 0
    dist_up, dist_down = 0, 0
    height = len(data)
    tree_altitude = data[i][j]
    for elt in data[i][j+1:]:
        dist_right += 1
        if elt >= tree_altitude:
            break
    for elt in data[i][j-1::-1]:
        dist_left += 1
        if elt >= tree_altitude:
            break
    for x in range(i+1, height):
        dist_down += 1
        if data[x][j] >= tree_altitude:
            break
    for x in range(i-1, -1, -1):
        dist_up += 1
        if data[x][j] >= tree_altitude:
            break
    return dist_down*dist_up*dist_left*dist_right
    
# Part 2
def part2(data):
    max_score = None
    for i in range(1, len(data)-1):
        for j in range(1, len(data[0])-1):
            if not max_score or scenic_score(data, i, j) > max_score:
                max_score = scenic_score(data, i, j)
                print(f"New high, {(i,j)=}, and {max_score=}")
    return max_score



assert part2(example_data) == 8

aocd.submit(part2(puzzle_data))
