from utils import * 

example_input = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""


def read_data(s):
    data = []
    for line in s.splitlines():
        l,r = line.split(',')
        data.append(([int(n) for n in l.split('-')], 
                    [int(n) for n in r.split('-')]))
    return data


example_data = read_data(example_input)
assert len(example_data) == 6
assert example_data[0] == ([2,4], [6,8])

# Part 1
def part1(data):
    overlaps = 0
    for ([l1, r1], [l2, r2]) in data:
        if (l1 <= l2 and r2 <= r1) or (l2 <= l1 and r1 <= r2):
            overlaps +=1 
    return overlaps


assert part1(example_data) == 2

puzzle_input = aocd.get_data(day=4)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    overlaps = 0
    for ([l1, r1], [l2, r2]) in data:
        if (l1 <= l2 <= r1) or (l1 <= r2 <= r1) \
            or (l2 <= l1 <= r2) or (l2 <= r1 <= r2):
            overlaps +=1 
    return overlaps


assert part2(example_data) == 4

aocd.submit(part2(puzzle_data))
