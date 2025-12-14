from functools import reduce
import operator


def read_line(txt: str):
    # Convert each diagram to a number (".#.." = 2)
    # Convert every wiring schematics to a number
    # Discard the rest
    pass
def read_data(txt: str):
    pass


raw_input_example = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""


def part1(data) -> int:
    # For every machine:
    # We'll start with a queue of [0], then iterate over every button, 
    # until we match the diagram of this machine, 
    # being careful not to compute multiple times the same diagram
    # For instance in the 1st machine, pushing the (3) and (2) buttons is the same as pushing (2,3)
    return 0

input_example = read_data(raw_input_example)
assert part1(input_example) == 0, "Error: part 1 on example"

input_data = read_data(open("input_10.txt").read())
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
