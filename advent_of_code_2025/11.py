from functools import cache


def read_data(txt: str):
    neighbours = {}
    for line in txt.strip().split("\n"):
        nodes = line.split()
        neighbours[nodes[0][:-1]] = nodes[1:]
    return neighbours


raw_input_example_part_1 = """aaa: you hhh
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
    distances = {start: 1}
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


input_example = read_data(raw_input_example_part_1)
assert part1(input_example) == 5, "Error: part 1 on example"

input_data = read_data(open("input_11.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")


def part2(g):
    a = count_paths(g, "svr", "fft")
    # I checked, there are no paths from "dac" to "fft"
    b = count_paths(g, "fft", "dac")
    c = count_paths(g, "dac", "out")
    return a * b * c


tests_part_2 = {  # example from website:
    """svr: aaa bbb
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
""": 2,
    # example I created
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
""": 30,  # 2 paths from svr to fft, 3 from fft to dac, and 5 from dac to out => 2 * 3 * 5 = 30 paths
}

for raw_test_data, expected in tests_part_2.items():
    test_graph = read_data(raw_test_data)
    assert part2(test_graph) == expected


def part2_dfs(g):
    @cache
    def dfs(node, fft, dac):
        if node == "out":
            return 1 if fft and dac else 0
        fft = fft or node == "fft"
        dac = dac or node == "dac"
        return sum(dfs(neighb, fft, dac) for neighb in g[node])

    return dfs("svr", False, False)


print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))

print(part2_dfs(input_data))
