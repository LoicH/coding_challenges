import itertools
import bisect
import tqdm


def read_data(txt: str):
    return [tuple(int(n) for n in l.split(",")) for l in txt.strip().split()]


puzzle_red_tiles = read_data(open("input_9.txt").read())


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


def rectangle_doesnt_intersect_rokil(
    min_i, max_i, min_j, max_j, horizontal_sides: dict, vertical_sides: dict
) -> bool:
    """Check if no side of the polygon intersects with this rectangle
    This DOES NOT check if the opposite corners (a and b) of the rectangle are red"""
    # Checking if a vertical side intersects the rectangle
    for i in range(min_i + 1, max_i):
        sides = vertical_sides.get(i, [])  # list of tuples (j1, j2)
        for j1, j2 in sides:
            if not (j2 <= min_j or max_j <= j1):
                return False

    # Check horizontal sides
    for j in range(min_j + 1, max_j):
        sides = horizontal_sides.get(j, [])
        for i1, i2 in sides:
            if not (i2 <= min_i or max_i <= i1):
                return False
    return True


def rectangle_doesnt_intersect_boojum(
    box1: tuple, box2: tuple, red_tiles: list
) -> bool:
    """Check if no side of the polygon intersects with this rectangle
    This DOES NOT check if the opposite corners (a and b) of the rectangle are red"""
    min_box_i, max_box_i = min(box1[0], box2[0]), max(box1[0], box2[0])
    min_box_j, max_box_j = min(box1[1], box2[1]), max(box1[1], box2[1])

    # Logic taken from https://reddit.com/r/adventofcode/comments/1phywvn/2025_day_9_solutions/nt2hps9/
    for a, b in itertools.pairwise(red_tiles + [red_tiles[0]]):
        min_line_i, max_line_i = min(a[0], b[0]), max(a[0], b[0])
        min_line_j, max_line_j = min(a[1], b[1]), max(a[1], b[1])
        if not (
            max_line_i <= min_box_i
            or min_line_i >= max_box_i
            or max_line_j <= min_box_j
            or min_line_j >= max_box_j
        ):
            return False
    return True


def part2(red_tiles):
    horiz, verti = sides(red_tiles)
    all_rectangles = []  # list of (corner1, corner2, area)
    for a, b in itertools.combinations(red_tiles, 2):
        area = (abs(a[0] - b[0]) + 1) * (abs(a[1] - b[1]) + 1)
        bisect.insort(all_rectangles, (a, b, area), key=lambda t: -t[2])

    # Let's iterate over the biggest rectangles, and find the first that is valid
    for a, b, area in tqdm.tqdm(all_rectangles):
        min_i, max_i = min(a[0], b[0]), max(a[0], b[0])
        min_j, max_j = min(a[1], b[1]), max(a[1], b[1])
        if rectangle_doesnt_intersect_rokil(min_i, max_i, min_j, max_j, horiz, verti):
            print(f"Big valid rectangle found with corners {a} and {b}")
            return area


import timeit

horiz, verti = sides(puzzle_red_tiles)
a, b = (4615, 66437), (94737, 50322)

min_i, max_i = min(a[0], b[0]), max(a[0], b[0])
min_j, max_j = min(a[1], b[1]), max(a[1], b[1])
print(
    timeit.timeit(
        lambda: rectangle_doesnt_intersect_rokil(min_i, max_i, min_j, max_j, horiz, verti), number=100
    )
)
# 0.8713879999704659
print(
    timeit.timeit(
        lambda: rectangle_doesnt_intersect_boojum(a, b, puzzle_red_tiles), number=100
    )
)
# 0.010588199831545353
print(part2(puzzle_red_tiles))  # Takes more than one minute
