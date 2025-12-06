def read_data(txt: str):
    pass


raw_input_example = """"""


def part1(data) -> int:
    return 0

input_example = read_data(raw_input_example)
assert part1(input_example) == 0, "Error: part 1 on example"

input_data = read_data(open("input_CHANGE_HERE.txt").read())
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
