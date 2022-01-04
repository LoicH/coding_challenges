from utils import *

# import sys

# sys.setrecursionlimit(5000)


def isin(a, b):
    """returns if cuboid A is entirely included in cuboid B"""
    return all(b[2 * i] <= a[2 * i] and b[2 * i + 1] >= a[2 * i + 1] for i in (0, 1, 2))


c1 = (-20, 26, -36, 17, -47, 7)
c2 = (-20, 33, -21, 17, -26, 7)
inter = (-20, 26, -21, 17, -26, 7)
assert isin(inter, c1)
assert not isin(c2, c1)


def test_intersect_range(a0, a1, b0, b1):
    """Return True if [a0, a1] intersects with [b0, b1]"""
    return b0 <= a0 <= b1 or b0 <= a1 <= b1 or a0 <= b0 <= a1 or a0 <= b1 <= a1


def intersect_range(a0, a1, b0, b1):
    """Return the range that is the intersection of [a0, a1] and [b0, b1]"""
    return (max(a0, b0), min(a1, b1))


def test_intersect_cuboid(a, b):
    """Cuboid A and cuboid B intersect if and only if their is an intersection on
    their x, y, and z ranges."""
    return all(test_intersect_range(a[i], a[i + 1], b[i], b[i + 1]) for i in (0, 2, 4))


def number_cubes(a):
    return math.prod(a[i + 1] - a[i] + 1 for i in (0, 2, 4))


def diff(a, b):
    """return the cuboids that make the difference of cuboid A minus cuboid B.
    i.e. diff(a, b) will return the cuboids that we can find in A but not in B"""
    # split on the X axis to create left and right cuboids
    # left is where X decrease and right is where X increase
    common_x = intersect_range(a[0], a[1], b[0], b[1])
    left = (a[0], common_x[0] - 1) + a[2:] if a[0] != common_x[0] else None
    right = (common_x[1] + 1, a[1]) + a[2:] if a[1] != common_x[1] else None

    # Now split the middle along the Y axis to create upper and lower cuboids
    # Bottom is or negative Y and Up is for positive Y
    common_y = intersect_range(a[2], a[3], b[2], b[3])
    lower = common_x + (a[2], common_y[0] - 1) + a[4:] if a[2] != common_y[0] else None
    upper = common_x + (common_y[1] + 1, a[3]) + a[4:] if a[3] != common_y[1] else None

    # And finish with splitting along the Z axis to have a front and back cuboids
    # Front is negative Z and Back is positive Z
    common_z = intersect_range(a[4], a[5], b[4], b[5])
    common = common_x + common_y
    front = common + (a[4], common_z[0] - 1) if a[4] != common_z[0] else None
    back = common + (common_z[1] + 1, a[5]) if a[5] != common_z[1] else None

    # Now return the real cuboids
    return [c for c in (left, right, lower, upper, front, back) if c]


def add_cuboid(to_add: tuple, there: list, mode: str):
    """Add (or remove) a cuboid to a list of lit cuboids.
    Parameters:

    - to_add: the coordinates of the cuboid, (x0, x1, y0, y1, z0, z1)
    - there: the list of the cuboids that are lit
    - mode: "on" or "off"
    """
    lit_cuboids = [to_add] if mode == "on" else []
    for cube_there in there:
        if not test_intersect_cuboid(to_add, cube_there):
            lit_cuboids.append(cube_there)
            continue
        diffs = diff(cube_there, to_add)
        lit_cuboids += diffs
    return lit_cuboids


# Part 1
def part1(data, max_range=50):
    lit_cubes = []
    max_cuboid = (-max_range, +max_range) * 3
    for line in tqdm(data):
        mode, to_add = line[0], tuple(line[1:])
        if not isin(to_add, max_cuboid):
            continue
        print(
            "Turning cuboid {} {} with a list of {} cuboids".format(
                to_add, mode, len(lit_cubes)
            )
        )
        lit_cubes = add_cuboid(to_add, lit_cubes, mode)
    return sum(number_cubes(c) for c in lit_cubes)


## Small tests
small_1 = ["on", 1, 4, 8, 9, 0, 0]
small_2 = ["on", 2, 3, 3, 4, 0, 0]
small_3 = ["on", 0, 5, 0, 2, 0, 0]
new_cube = ["on", 2, 3, 2, 11, 0, 0]

small_2_cubes_layout = [small_1, new_cube]
assert part1(small_2_cubes_layout) == 24

small_3_cubes_layout = [small_1, small_2, new_cube]
assert part1(small_3_cubes_layout) == 24

assert part1([small_3, new_cube]) == 36
assert part1([new_cube, small_2]) == 20

small_4_cubes_layout = [small_1, small_2, small_3, new_cube]
assert part1(small_4_cubes_layout) == 40

new_off_cube = ["off"] + new_cube[1:]
assert part1([small_1, new_off_cube]) == 4
assert part1([small_1, small_2, new_off_cube]) == 4
assert part1([small_1, small_2, small_3, new_off_cube]) == 20
assert part1([small_1, small_3, new_off_cube, small_2]) == 24

small_example_input = """on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"""


def read_data(s):
    pattern = r"(-?\d+)"
    return [
        [line.split()[0]] + [int(c) for c in re.findall(pattern, line)]
        for line in s.splitlines()
    ]


small_example_data = read_data(small_example_input)
assert part1(small_example_data) == 39


with open("example_input_22.txt", "r") as fp:
    example_input = fp.read()


example_data = read_data(example_input)
assert len(example_data) == 22
assert example_data[0] == ["on", -20, 26, -36, 17, -47, 7]
assert part1(example_data) == 590784

puzzle_input = aocd.get_data(day=22)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data), day=22)
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return part1(data, max_range=math.inf)


with open("example_input_22_2.txt", "r") as fp:
    example_input_2 = fp.read()
example_data_2 = read_data(example_input_2)
p2 = part2(example_data_2)
assert p2 == 2758514936282235

puzzle_p2 = part2(puzzle_data)
aocd.submit(puzzle_p2, day=22)
