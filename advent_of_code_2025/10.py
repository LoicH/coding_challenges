from functools import reduce
import operator
import bisect
from os import curdir
import tqdm


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


def match_joltage(goal: list, buttons: list) -> int:
    start = [0] * len(goal)
    queue_to_visit = [(start, 0, dist(start, goal))]
    already_seen = {tuple(start)}
    buttons_binary = []
    for b in buttons:
        new_button = [0] * len(goal)
        for i in b:
            new_button[i] = 1
        buttons_binary.append(new_button)
    while queue_to_visit:  # for _ in range(max(goal) * len(buttons)):
        current_joltage, steps, cur_dist = queue_to_visit[0]
        print(
            f"{len(queue_to_visit)} elements to check, already seen {len(already_seen)} joltages, current distance = {cur_dist}"
        )
        queue_to_visit = queue_to_visit[1:]
        for b in buttons_binary:
            new_joltage = [n + b[i] for i, n in enumerate(current_joltage)]
            if new_joltage == goal:
                return steps + 1
            # Stop early if we get one joltage requirement that is too big
            too_big = any(new_joltage[i] > req for i, req in enumerate(goal))
            if too_big:
                continue
            t = tuple(new_joltage)
            if t in already_seen:
                continue
            already_seen.add(t)
            elt_to_add = (new_joltage, steps + 1, dist(new_joltage, goal))
            bisect.insort(queue_to_visit, elt_to_add, key=lambda t: t[2])
    raise ValueError(f"Can't reach {goal=}")


assert (
    match_joltage([3, 5, 4, 7], [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]]) == 10
), "Error on the first line of example"
assert match_joltage([0, 100, 5, 5, 0], [[0, 1], [1], [2], [3], [4]]) == 110


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
