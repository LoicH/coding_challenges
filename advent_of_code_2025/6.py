from functools import reduce
import operator
import numpy as np


def read_data(txt: str):
    lines = txt.strip().split("\n")
    ops = [
        (i, c) for (i, c) in enumerate(lines[-1]) if not c.isspace()
    ]  # [(0,'*'), (4,'+'), (8,'*'), (11,'+')] for instance
    numbers = []
    # col_sep_idx = [i for (i,_) in ops]
    for l in lines[:-1]:
        numbers.append([])
        for problem_idx, (col_start, _) in enumerate(ops):
            if problem_idx == len(ops) - 1:
                col_end = len(l)
            else:
                col_end = ops[problem_idx+1][0]-1
            numbers[-1].append(l[col_start:col_end])

    return numbers, [c for (_, c) in ops]


raw_input_example = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""

print("Running tests for read_data function...")
input_example = read_data(raw_input_example)
numbers_example, ops_example = input_example
assert ops_example == ["*", "+", "*", "+"]
assert numbers_example[0] == ["123", "328", " 51" ,"64 "]


def part1(data) -> int:
    numbers, ops = data
    total = 0
    for i, op in enumerate(ops):
        problems_numbers = [int(l[i]) for l in numbers]
        if op == "+":
            total += sum(problems_numbers)
        elif op == "*":
            total += reduce(operator.mul, problems_numbers)
    return total


assert part1(input_example) == 4277556, "Error: part 1 on example"

input_data = read_data(open("input_6.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")


def read_column_part2(number_column):
    """Read a column like ["123", "45", "6"] => [356, 24, 1]"""
    longest = max(len(s) for s in number_column)
    worksheet = np.array([list(s.ljust(longest)) for s in number_column])
    col = []
    for line in worksheet.T:
        col.append(int("".join(c for c in line if c != " ")))
    return col[::-1]


tests_read_col = [
    (["123", " 45", "  6"], [356, 24, 1]),
    (["10", " 1"], [1, 1]),
    ([" 1", "10"], [10, 1]),
    (["10", " 1", "10"], [10, 11]),
    (["328", "64 ", "98 "], [8, 248, 369]),
]

print("Testing the read_column_part2 function...")
for (col, expected) in tests_read_col:
    res = read_column_part2(col)
    assert (
        res == expected
    ), f"Wrong result when reading column {col}: got {res} instead of {expected}"
print("All good!")


def part2(data):
    numbers, ops = data
    total = 0
    for i, op in enumerate(ops):
        problem_column = [l[i] for l in numbers]
        problems_numbers = read_column_part2(problem_column)
        res = 0
        if op == "+":
            res = sum(problems_numbers)
        elif op == "*":
            res = reduce(operator.mul, problems_numbers)
        total += res
    return total


test_cases_part_2 = {
    raw_input_example: 3263827,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))
