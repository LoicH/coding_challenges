from functools import reduce
import operator
import itertools
import tkinter
import bisect


def read_data(txt: str):
    return [tuple(int(n) for n in l.split(",")) for l in txt.strip().split()]


raw_input_example = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""


def part1(data) -> int:
    max_area = 0
    for a, b in itertools.combinations(data, 2):
        area = (abs(a[0] - b[0]) + 1) * (abs(a[1] - b[1]) + 1)
        if area > max_area:
            max_area = area
            # print(f"New max area found ({area}) with {(a,b)=}")
    return max_area


example_data = read_data(raw_input_example)
assert part1(example_data) == 50, "Error: part 1 on example"

puzzle__data = read_data(open("input_9.txt").read())
print(f"Result on puzzle input: {part1(puzzle__data)}")


def border_tiles(data):
    border = set()
    for a, b in zip(data, data[1:] + [data[0]]):
        a, b = min(a, b), max(a, b)
        for i in range(a[0], b[0] + 1):
            for j in range(a[1], b[1] + 1):
                border.add((i, j))
    return border


def compute_bounds(red_tiles: list):
    f = red_tiles[0]
    # `i` is the column index while `j` is the line index
    min_i, min_j, max_i, max_j = f[0], f[1], f[0], f[1]
    for i, j in red_tiles[1:]:
        min_i = min(min_i, i)
        max_i = max(max_i, i)
        min_j = min(min_j, j)
        max_j = max(max_j, j)
    return min_i, min_j, max_i, max_j


def find_inside_border(border, verbose=False):
    horizontal_crossings = {}  # dict of {j: [list of i]}
    for i, j in border:
        if j not in horizontal_crossings:
            horizontal_crossings[j] = [i]
        elif len(horizontal_crossings[j]) < 3:
            # We only want the lines with 2 crossings.
            # As soon as we reach 3 `i`, we stop storing them
            bisect.insort(horizontal_crossings[j], i)
    if verbose:
        print(f"Found crossings for {len(horizontal_crossings)} lines.")
    # horizontal_crossings = {j:l for (j,l) in horizontal_crossings.items() if len(l) == 2}
    # We can add the right of the 1st crossing and the left of the 2nd to start our inside border
    to_visit = set()
    for j, l in horizontal_crossings.items():
        if len(l) != 2:
            continue
        [i_l, i_r] = l
        to_visit.update({(i_l + 1,j), (i_r - 1,j)})

    if verbose:
        print(f"Got {len(to_visit)} points to visit")

    inside = set()
    deltas = [(-1,0), (+1,0), (0,-1), (0,-1)]
    while len(to_visit) > 0:
        (i,j) = to_visit.pop()
        inside.add((i,j))
        neighbours = {(i+di, j+dj) for di,dj in deltas}
        # Remove the neighbours we already visited
        neighbours -= inside
        neighbours -= border
        if verbose:
            print(f"Current = {(i,j)}, only {neighbours} are not already inside or on a border")
        # Then add the remaining neighbours to the stack of the points to visit
        to_visit.update(neighbours)
    return inside




def display(data, inside_border=None):
    # lines = []
    max_size = 1024
    window = tkinter.Tk()

    canvas = tkinter.Canvas(
        window, width=max_size, height=max_size, background="#121212"
    )
    canvas.pack()
    max_bound = max(compute_bounds(data))
    shrink_factor = max(max_bound // max_size, 1)
    for a, b in zip(data, data[1:] + [data[0]]):
        coord = [n // shrink_factor for n in [*a, *b]]
        canvas.create_line(*coord, fill="#905888")
    if inside_border:
        for (i,j) in inside_border:
            canvas.create_rectangle(i,j, i+1, j+1, fill="#628141")
    window.mainloop()
    # for j in range(min_j, max_j + 1):
    #     line = ["X" if (i, j) in border else "." for i in range(min_i, max_i + 1)]
    #     with open("display.txt", "a") as fp:
    #     # print("".join(line))
    #         fp.write("".join(line) + "\n")


example_bounds = compute_bounds(example_data)
example_border = border_tiles(example_data)
example_inside_border = find_inside_border(example_border, verbose=True)
display(example_data, inside_border=example_inside_border)
# display(data=input_example)

puzzle_bounds = compute_bounds(puzzle__data)
puzzle_border = border_tiles(puzzle__data)
puzzle_inside_border = find_inside_border(puzzle_border, verbose=True)
print(puzzle_bounds)
# All tiles are in a square of 100,000 x 100,000 => 10^10 pixels
# Tuple (100000, 100000) uses 56 bytes in memory => 5.6 * 10^11 = 560 GB...
# I can't store a hashmap of all (or half) the coordinates to indicate if they are green or not

# What if I just store the coordinates of tiles that are "just inside"?
# That means I can iterate over the red tiles
# For every "inside" neighbour, I start searching for big rectangles


display(data=puzzle__data, inside_border=puzzle_inside_border)


def hashmap_tiles(data):
    border = border_tiles(data)
    ok_tiles = set()
    min_i, min_j, max_i, max_j = compute_bounds(data)
    # Let's explore the tiles diagonally to find all the tiles that are inside
    for k in range(min_i + min_j, max_i + max_j + 1):
        inside = False
        for i in range(max(min_i, k - max_j), max_i + 1):
            j = k - i
            if j < min_j or j > max_j:
                break
            if (i, j) in border:
                inside = not inside
            if inside:
                ok_tiles.add((i, j))
    return ok_tiles


print("Tests for hashmap_tiles...")
assert hashmap_tiles([(0, 0), (0, 3)]) == {(0, 0), (0, 1), (0, 2), (0, 3)}
assert (
    len(hashmap_tiles(example_data)) == 46
), "Error, we should have 46 red or green tiles"


def part2(data):
    red_tiles = set(data)
    ok_tiles = hashmap_tiles(data)
    min_i, min_j, max_i, max_j = compute_bounds(data)

    max_area = 0
    for ai, aj in red_tiles:
        min_bi = min_i
        max_bi = max_i
        # Exploring the lines above `(ai, aj)`
        for bj in range(aj, min_j - 1, -1):
            # Exploring the columns on the left
            for bi in range(ai, min_i - 1, -1):
                if (bi, bj) not in ok_tiles:
                    min_bi = bi
                if bi <= min_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
            # Exploring the columns on the right
            for bi in range(ai, max_i + 1):
                if (bi, bj) not in ok_tiles:
                    max_bi = bi
                if bi >= max_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
        # Explore the lines under (ai, aj)
        min_bi = min_i
        max_bi = max_i
        for bj in range(aj, max_j + 1):
            # Exploring on the left
            for bi in range(ai, min_i - 1, -1):
                if (bi, bj) not in ok_tiles:
                    min_bi = bi
                if bi <= min_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
            # Exploring on the right
            for bi in range(ai, max_i + 1):
                if (bi, bj) not in ok_tiles:
                    max_bi = bi
                if bi >= max_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
    return max_area


test_cases_part_2 = {
    raw_input_example: 24,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}, {computed=} while {expected=}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(puzzle__data))
