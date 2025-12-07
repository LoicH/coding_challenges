def read_data(txt: str):
    return txt


raw_input_example = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""


def part1(data) -> int:
    beams = {data.index('S')}
    splits = 0
    for line in data.split()[1:]:
        new_beams = set()
        for b in beams:
            if line[b] == '.':
                new_beams.add(b)
            else:
                new_beams.update({b-1, b+1})
                splits += 1
        beams = new_beams
    return splits



input_example = read_data(raw_input_example)
assert part1(input_example) == 21, "Error: part 1 on example"

input_data = read_data(open("input_7.txt").read())
print(f"Result on puzzle input: {part1(input_data)}")


def part2(data):
    start = data.index('S')
    lines = data.split() # List[str]
    timelines = [[0] * len(lines[0]) for _ in range(len(lines))]
    timelines[0][start] = 1
    for j, line in enumerate(lines[1:]): #j:[0 - height-2]
        # new_beams = set()
        for i, n in enumerate(timelines[j]):
            if n == 0:
                continue
            if line[i] == '.':
                timelines[j+1][i] += n
            elif line[i] == '^':
                timelines[j+1][i-1] += n 
                timelines[j+1][i+1] += n 
    return sum(timelines[-1])

test_cases_part_2 = {
    raw_input_example: 40,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input))
    assert computed == expected, f"Failed test #{i+1}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data))
