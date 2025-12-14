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


example_red_tiles = read_data(raw_input_example)
# assert part1(example_data) == 50, "Error: part 1 on example"

puzzle_red_tiles = read_data(open("input_9.txt").read())
# print(f"Result on puzzle input: {part1(puzzle__data)}")


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
        new_coords = [int(n * tile_size) for n in a + b]
        canvas.create_line(*new_coords, fill="#E42D9B")

    rectangles = [
        # Wrong rectangles
        # ((98274, 51571), (54547, 98172), "#FC6EC3"),
        # ((98274, 51571), (50875, 98306), "#FCEEEE"),
        ((4615, 66437) , (94737, 50322), "#FCEEEE")
    ]
    for a, b, col in rectangles:
        new_coords = [int(n * tile_size) for n in a + b]
        canvas.create_rectangle(*new_coords, outline=col)

    window.mainloop()


# puzzle_border = border_tiles(puzzle_red_tiles)
display(puzzle_red_tiles)


def rectangle_doesnt_intersect(box1: tuple, box2: tuple, red_tiles: list) -> bool:
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


assert not rectangle_doesnt_intersect((2, 5), (11, 1), example_red_tiles)
assert rectangle_doesnt_intersect((2, 5), (9, 7), example_red_tiles)
assert rectangle_doesnt_intersect((2, 5), (7, 3), example_red_tiles)
test_red_tiles = [(2, 10), (6, 10), (6, 14), (2, 14)]  # 5x5 square
assert rectangle_doesnt_intersect((2, 10), (6, 14), test_red_tiles)


# All tiles are in a square of 100,000 x 100,000 => 10^10 tiles
# Tuple (100000, 100000) uses 56 bytes in memory => 5.6 * 10^11 = 560 GB...
# I can't store a hashmap of all (or half) the coordinates to indicate if they are green or not
def part2(red_tiles):
    all_rectangles = []  # list of (corner1, corner2, area)
    for a, b in itertools.combinations(red_tiles, 2):
        area = (abs(a[0] - b[0]) + 1) * (abs(a[1] - b[1]) + 1)
        bisect.insort(all_rectangles, (a, b, area), key=lambda t: -t[2])

    # Let's iterate over the biggest rectangles, and find the first that is valid
    for a, b, area in tqdm.tqdm(all_rectangles):
        if rectangle_doesnt_intersect(a, b, red_tiles):
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
print(part2(puzzle_red_tiles))
