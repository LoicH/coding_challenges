from functools import reduce
import operator



def read_data(txt: str):
    neighbours = {}
    for line in txt.strip().split("\n"):
        nodes = line.split()
        neighbours[nodes[0][:-1]] = nodes[1:] 
    return neighbours


raw_input_example = """aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""


def count_paths(graph, start, finish) -> int:
    distances = {start:1}
    queue = [start]
    visited = set([start])
    while len(queue) > 0:
        current = queue[0]
        queue = queue[1:]
        cur_dist = distances[current]
        for next in graph.get(current, []):
            distances[next] = distances.get(next, 0) + cur_dist
            if next != finish and next not in visited:
                queue.append(next)
                visited.add(next)
    return distances[finish]

def part1(g):
    return count_paths(g, "you", "out")

input_example = read_data(raw_input_example)
assert part1(input_example) == 5, "Error: part 1 on example"

input_data = read_data(open("input_11.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")

def part2(g):
    a = count_paths(g, "svr", "fft")
    b = count_paths(g, "fft", "dac")
    c = count_paths(g, "dac", "out")
    return a*b*c

tests_part_2 = {"""svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
""":2,
"""svr: a b
a: fft
b: fft
fft: d e f 
d: dac
e: dac
f: dac
dac: g h i j k
g: out
h: out
i: out
j: out
k: out
""": 30
}

for raw_test_data, expected in tests_part_2.items():
    test_graph = read_data(raw_test_data)
    assert part2(test_graph) == expected


print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))
# 12359294262812 too low