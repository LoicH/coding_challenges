from functools import reduce
import operator
import numpy as np
import itertools


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
            print(f"New max area found ({area}) with {(a,b)=}")
    return max_area


input_example = read_data(raw_input_example)
assert part1(input_example) == 50, "Error: part 1 on example"

input_data = read_data(open("input_9.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")

def hashmap_tiles(data):
    ok_tiles = set()
    for (a, b) in zip(data, data[1:] + [data[0]]):
        a, b = min(a, b), max(a, b)
        for i in range(a[0], b[0] + 1):
            for j in range(a[1], b[1] + 1):
                ok_tiles.add((i, j))
    return ok_tiles

def compute_bounds(red_tiles: list):
    f = red_tiles[0]
    min_i, min_j, max_i, max_j = f[0], f[1], f[0], f[1]
    for (i,j) in red_tiles[1:]:
        min_i = min(min_i, i)
        max_i = max(max_i, i)
        min_j = min(min_j, j)
        max_j = max(max_j, j)
    return min_i, min_j, max_i, max_j

print("Tests for hashmap_tiles...")
assert hashmap_tiles([(0,0), (0, 3)]) == {(0,0), (0,1), (0,2), (0,3)}
assert hashmap_tiles(input_example) == 46, "Error, we should have 46 red or green tiles"

def part2(data):
    red_tiles = set(data)
    ok_tiles = hashmap_tiles(data)
    min_i, min_j, max_i, max_j = compute_bounds(data)

    max_area = 0
    for (ai, aj) in red_tiles:
        # Exploring to the left
        min_bi = min_i
        max_bi = max_i
        for bj in range(aj, min_j - 1, -1):
            # Exploring upper left
            for bi in range(ai, min_i - 1, -1):
                if (bi, bj) not in ok_tiles:
                    min_bi = bi
                if bi <= min_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
            # Exploring lower left
            for bi in range(ai, max_i + 1):
                if (bi, bj) not in ok_tiles:
                    max_bi = bi
                if bi >= max_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
        # Explore the right side
        min_bi = min_i
        max_bi = max_i
        for bj in range(aj, max_j + 1):
            # Exploring upper right
            for bi in range(ai, min_i - 1, -1):
                if (bi, bj) not in ok_tiles:
                    min_bi = bi
                if bi <= min_bi:
                    break
                if (bi, bj) in red_tiles:
                    area = (abs(ai - bi) + 1) * (abs(aj - bj) + 1)
                    max_area = max(max_area, area)
            # Exploring lower right
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
print(part2(input_data))
