from functools import reduce
import math
import operator
import bisect
from os import curdir
import tqdm
import z3

def read_line(line: str):
    # Convert each diagram to a number (".#.." = 2)
    words = line.strip().split()
    diagram = words[0][1:-1]
    # '.#..' = 2
    diagram_value = sum(2**i for (i, c) in enumerate(diagram) if c == "#")

    schematics = [[int(c) for c in w[1:-1].split(",")] for w in words[1:-1]]

    joltage_goal = [int(n) for n in words[-1][1:-1].split(",")]
    return diagram_value, schematics, joltage_goal


assert read_line("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}") == (
    6,
    [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]],
    [3, 5, 4, 7],
)


def read_data(txt: str):
    return [read_line(l) for l in txt.strip().split("\n")]


raw_input_example = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""


def match_lights(diagram_value, schematics: list) -> int:
    # We'll start with a queue of [0], then iterate over every button,
    # until we match the diagram of this machine,
    # being careful not to compute multiple times the same diagram
    # For instance in the 1st machine, pushing the (3) and (2) buttons is the same as pushing (2,3)
    schematics_values = [sum(2**n for n in s) for s in schematics]
    queue = [(0, 0)]  # diagram value, number of buttons pushed
    already_seen = {0}
    for _ in range(2 ** len(schematics_values)):
        current_light, steps = queue[0]
        queue = queue[1:]
        for button in schematics_values:
            new_light = current_light ^ button
            if new_light == diagram_value:
                return steps + 1
            elif new_light in already_seen:
                continue
            already_seen.add(new_light)
            queue.append((new_light, steps + 1))
    raise ValueError(f"Can't match lights with value {diagram_value}")


assert match_lights(6, [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]]) == 2


def part1(machines) -> int:
    return sum(match_lights(d, s) for d, s, _ in machines)


input_example = read_data(raw_input_example)
assert part1(input_example) == 7, "Error: part 1 on example"

input_data = read_data(open("input_10.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")


def dist(a: list, b: list) -> int:
    return sum(abs(i - j) for i, j in zip(a, b))


def match_joltage(goals: list, buttons: list) -> int:
    
    coefs = [z3.Int(chr(ord('a')+n)) for n,_ in enumerate(buttons)]
    constraints_sum = []
    for i,goal in enumerate(goals):
        # Search for all buttons that increment this value
        contraint = [coefs[j] for j,button in enumerate(buttons) if i in button]
        constraints_sum.append(sum(contraint) == goal)
    constraints = constraints_sum + [c >= 0 for c in coefs]
    solve = z3.Solver()
    solve.add(*constraints)
    max_search = sum(goals)
    min_search = 1
    while min_search < max_search-1:
        solve.push()
        current = int(math.floor((max_search + min_search)/2))
        solve.add(sum(coefs) < current)
        if solve.check() == z3.sat:
            max_search = current
        else:
            min_search = current
        solve.pop() 
    return min_search


assert (
    match_joltage([3, 5, 4, 7], [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]]) == 10
), "Error on the first line of example"
# Explanation: To achieve the goals [100, 200, 5, 5, 0] with the given button mappings,
# the minimal total number of button presses is 210 
# (press button 0+1 100 times, button 1 100 times, button 2 5 times, button 3 5 times, button 4 0 times).
assert match_joltage([100, 200, 5, 5, 0], [[0, 1], [1], [2], [3], [4]]) == 210
assert match_joltage([100, 100, 100, 100, 100], [[0, 1], [1], [2, 3], [3], [4]]) == 300
from timeit import timeit

print(
    timeit(
        lambda: match_joltage([100, 200, 5, 5, 0], [[0, 1], [1], [2], [3], [4]]),
        number=10,
    )
)
print(
    timeit(
        lambda: match_joltage(
            [100, 100, 100, 100, 100], [[0, 1, 2], [1], [2, 3, 4], [3], [4]]
        ),
        number=10,
    )
)

print(
    match_joltage(
        [56, 49, 25, 47, 23, 67, 40, 13, 63, 54],
        [
            [0, 1, 3, 4, 5, 6, 8, 9],
            [0, 2, 3, 4, 6, 7, 8, 9],
            [9],
            [0, 1, 2, 6, 7],
            [4, 5],
            [1, 2, 5, 8, 9],
            [1, 5, 6, 8],
            [0, 3, 5, 9],
            [0, 1, 2, 5, 7, 8],
            [0, 3, 5, 8],
        ],
    )
)
# Finding even one joltage is soooo long...

def part2(machines):
    total = 0
    for _, b, j in tqdm.tqdm(machines):
        total += match_joltage(j, b)
    return total


test_cases_part_2 = {
    raw_input_example: 33,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))
