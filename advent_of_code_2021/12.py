import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

example_inputs = {
    """start-A
start-b
A-c
A-b
b-d
A-end
b-end""": (
        10,
        36,
    ),
    """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc""": (
        19,
        103,
    ),
    """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW""": (
        226,
        3509,
    ),
}


def read_data(s):
    graph = defaultdict(set)
    for line in s.splitlines():
        a, b = line.split("-")
        if a == "start":
            graph[a].add(b)
        elif b == "start":
            graph[b].add(a)
        elif a == "end":
            graph[b].add(a)
        elif b == "end":
            graph[a].add(b)
        else:
            graph[a].add(b)
            graph[b].add(a)
    return graph


# Part 1
def part1(data):
    n_paths = 0
    paths = [[node] for node in data["start"]]
    while len(paths) > 0:
        path = paths.pop()
        if path[-1] == "end":
            n_paths += 1
        elif path[-1] == path[-1].lower() and path[-1] in path[:-1]:
            continue
        else:
            paths += [path + [node] for node in data[path[-1]]]
    return n_paths


for i, (input_example, (expected_val, _)) in enumerate(example_inputs.items()):
    example_data = read_data(input_example)
    val = part1(example_data)
    assert val == expected_val, "Wrong output for example {}, {} != {}".format(
        i + 1, val, expected_val
    )

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
# aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    n_paths = 0
    paths = [([node], True) for node in data["start"]]
    while len(paths) > 0:
        path, only_once = paths.pop()
        if path[-1] == "end":
            n_paths += 1
        elif path[-1] == path[-1].lower() and path[-1] in path[:-1]:
            if only_once:
                paths += [(path + [node], False) for node in data[path[-1]]]
        else:
            paths += [(path + [node], only_once) for node in data[path[-1]]]
    return n_paths


for i, (input_example, (_, expected_val)) in enumerate(example_inputs.items()):
    example_data = read_data(input_example)
    val = part2(example_data)
    assert val == expected_val, "Wrong output for example {}, {} != {}".format(
        i + 1, val, expected_val
    )


aocd.submit(part2(puzzle_data))
