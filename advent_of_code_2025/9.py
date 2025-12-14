from functools import reduce
import operator
import itertools
import tkinter
import bisect
import tqdm


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
# assert part1(example_data) == 50, "Error: part 1 on example"

puzzle_data = read_data(open("input_9.txt").read())
# print(f"Result on puzzle input: {part1(puzzle__data)}")


def sides(data):
    vertical = {}
    horizontal = {}
    for a, b in zip(data, data[1:] + [data[0]]):
        a, b = min(a, b), max(a, b)
        # Either a[0] < b[0] or (a[0] == b[0] and a[1] < b[1])
        if a[0] < b[0]:
            # Horizontal line
            j = a[1]
            horizontal[j] = horizontal.get(j, []) + [(a[0], b[0])]
        else:
            # Vertical line
            i = a[0]
            vertical[i] = vertical.get(i, []) + [(a[1], b[1])]
    return horizontal, vertical


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


def border_tiles(data):
    border = set()
    for a, b in zip(data, data[1:] + [data[0]]):
        a, b = min(a, b), max(a, b)
        for i in range(a[0], b[0] + 1):
            for j in range(a[1], b[1] + 1):
                border.add((i, j))
    return border

def display(red_tiles):
    max_size = 800
    window = tkinter.Tk()

    canvas = tkinter.Canvas(
        window, width=max_size, height=max_size, background="#121212"
    )
    canvas.pack()
    max_bound = max(max(a, b) for a, b in red_tiles)
    # if max is 10, we can multiplicate everything by 102 (max_size / max_bound) to fill the window
    # if max is 10 000 we need to divide by 10 (max_bound / max_size), i.e. multiplicate by max_size/max_bound
    tile_size = max_size / max_bound
    # To debug
    canvas.create_rectangle(0, 0, 0 - tile_size, 0 - tile_size, fill="#37AC14")
    canvas.create_rectangle(tile_size, 0, 2 * tile_size, 0 - tile_size, fill="#37AC14")

    
    for a, b in zip(red_tiles, red_tiles[1:] + [red_tiles[0]]):
        new_coords = [int(n * tile_size) for n in a+b]
        canvas.create_line(*new_coords,  fill="#E42D9B")
    
    rectangles = [((98274, 51571), (54547, 98172), "#FC6EC3"), ((98274, 51571), (50875, 98306), "#FCEEEE")]
    for a,b,col in rectangles:
        new_coords = [int(n * tile_size) for n in a+b]
        canvas.create_rectangle(*new_coords,  outline=col)

    window.mainloop()

puzzle_border = border_tiles(puzzle_data)
# display(puzzle_data)


example_bounds = compute_bounds(example_data)
horiz_example, vert_example = sides(example_data)
assert vert_example == {
    2: [(3, 5)],
    7: [(1, 3)],
    9: [(5, 7)],
    11: [(1, 7)],
}, f"Error, {sorted(vert_example.items())}"

test_data = [(2, 10), (6, 10), (6, 14), (2, 14)]  # 5x5 square
test_bounds = compute_bounds(test_data)
horiz_test, vert_test = sides(test_data)
assert horiz_test == {10: [(2, 6)], 14: [(2, 6)]}


def rectangle_has_outside_tile(
    a: tuple, b: tuple, horizontal_sides: dict, vertical_sides: dict
) -> bool:
    """Check if the given rectangle has tiles that are outside of the polygon"""
    min_i, max_i = min(a[0], b[0]), max(a[0], b[0])
    min_j, max_j = min(a[1], b[1]), max(a[1], b[1])
    # Laser coming from the left
    # We check if there are points on the left side of the rectangle
    # that did receive a laser that did not go through a single vertical side
    no_sides = set(range(min_j + 1, max_j))
    one_sides = set()
    for i in range(0, min_i+1 ):
        # j indices on the left side of the rectangle
        # that did not receive a laser that passed through one vertical side
        for j1, j2 in vertical_sides.get(i, []):
            to_remove = set() # I don't want to change the 'one_sides' set during iteration
            for j in one_sides:
                if j1 <= j <= j2:
                    # We don't care about lasers that pass through more than one side
                    to_remove.add(j)
            one_sides -= to_remove
            for j in no_sides:
                if j1 <= j <= j2:
                    one_sides.add(j)
            no_sides -= one_sides
    if no_sides:
        return True
    # Laser coming from the right
    no_sides = set(range(min_j + 1, max_j))
    one_sides = set()
    for i in range(max(vertical_sides.keys())+1, max_i-1, -1):
        for j1, j2 in vertical_sides.get(i, []):
            to_remove = set() # I don't want to change the 'one_sides' set during iteration
            for j in one_sides:
                if j1 <= j <= j2:
                    # We don't care about lasers that pass through more than one side
                    to_remove.add(j)
            one_sides -= to_remove
            for j in no_sides:
                if j1 <= j <= j2:
                    one_sides.add(j)
            no_sides -= one_sides
    if no_sides:
        return True
    return False


assert rectangle_has_outside_tile((2, 5), (11, 1), horiz_example, vert_example)
assert rectangle_has_outside_tile((2, 5), (9, 7), horiz_example, vert_example)
assert not rectangle_has_outside_tile((2, 5), (7, 3), horiz_example, vert_example)
assert not rectangle_has_outside_tile((2, 10), (6, 14), horiz_test, vert_test)


def rectangle_doesnt_intersect(
    a: tuple, b: tuple, horizontal_sides: dict, vertical_sides: dict
) -> bool:
    """Check if no side of the polygon intersects with this rectangle
    This DOES NOT check if the opposite corners (a and b) of the rectangle are red"""
    min_i, max_i = min(a[0], b[0]), max(a[0], b[0])
    min_j, max_j = min(a[1], b[1]), max(a[1], b[1])

    # Checking if a vertical side intersects the rectangle
    for i in range(min_i + 1, max_i):
        sides = vertical_sides.get(i, [])  # list of tuples (j1, j2)
        for j1, j2 in sides:
            if (j1 <= min_j and j2 > min_j) or (j1 <= max_j and j2 > max_j):
                return False

    # Check horizontal sides
    for j in range(min_j + 1, max_j):
        sides = horizontal_sides.get(j, [])
        for i1, i2 in sides:
            if (i1 <= min_i and i2 > min_i) or (i1 <= max_i and i2 > max_i):
                return False
    return True


assert not rectangle_doesnt_intersect((2, 5), (11, 1), horiz_example, vert_example)
assert rectangle_doesnt_intersect((2, 5), (9, 7), horiz_example, vert_example)
assert rectangle_doesnt_intersect((2, 5), (7, 3), horiz_example, vert_example)
assert rectangle_doesnt_intersect((2, 10), (6, 14), horiz_test, vert_test)


# All tiles are in a square of 100,000 x 100,000 => 10^10 tiles
# Tuple (100000, 100000) uses 56 bytes in memory => 5.6 * 10^11 = 560 GB...
# I can't store a hashmap of all (or half) the coordinates to indicate if they are green or not
def part2(data):
    horiz, verti = sides(data)
    all_rectangles = []  # list of (corner1, corner2, area)
    for a, b in itertools.combinations(data, 2):
        area = (abs(a[0] - b[0]) + 1) * (abs(a[1] - b[1]) + 1)
        bisect.insort(all_rectangles, (a, b, area), key=lambda t: -t[2])

    # Let's iterate over the biggest rectangles, and find the first that is valid
    for a, b, area in tqdm.tqdm(all_rectangles):
        if area > 2215286400:
            continue # ...
        if rectangle_doesnt_intersect(a, b, horiz, verti) and not rectangle_has_outside_tile(
            a, b, horiz, verti
        ):
            print(f"Big valid rectangle found with corners {a} and {b}")
            return area


test_cases_part_2 = {
    raw_input_example: 24,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}, {computed=} while {expected=}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(puzzle_data))  
# I know that 2215286400 is too high...
# and 1437176532 too low...
