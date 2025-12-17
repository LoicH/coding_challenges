def read_data(txt: str):
    blocks = txt.strip().split("\n\n")
    shapes = [b[3:] for b in blocks[:-1]]
    regions = []
    for line in blocks[-1].split("\n"):
        # line = "NNNxNNN: NN NN NN NN"
        words = line.split()
        shape = [int(n) for n in words[0][:-1].split("x")]
        quantities = [int(n) for n in words[1:]]
        regions.append((shape, quantities))
    return shapes, regions


raw_input_example = """0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"""


def has_enough_tiles(region_shape, quantities, shapes_size):
    region_tiles = region_shape[0] * region_shape[1]
    needed_tiles = sum(q * s for q, s in zip(quantities, shapes_size))
    return region_tiles >= needed_tiles


def part1(data) -> int:
    shapes, regions = data
    shapes_sizes = [s.count("#") for s in shapes]
    return sum(
        has_enough_tiles(r_shape, r_qties, shapes_sizes) for r_shape, r_qties in regions
    )


input_example = read_data(raw_input_example)
assert part1(input_example) == 2, "Error: part 1 on example"

input_data = read_data(open("input_12.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")


def part2(inp):
    return 0


test_cases_part_2 = {
    raw_input_example: 0,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))
