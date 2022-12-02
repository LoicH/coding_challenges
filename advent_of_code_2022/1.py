import aocd

test_input = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

def read_data(s):
    return [[int(n) for n in elve.split()] for elve in s.split("\n\n")]


def part_1(elves):
    return max(sum(e) for e in elves)

test_data = read_data(test_input)
assert part_1(test_data) == 24000

with open("input_1.txt") as fp:
    input_data = read_data(fp.read())

aocd.submit(part_1(input_data), part=1)

def part_2(elves):
    return sum(sorted(sum(e) for e in elves)[-3:])

assert part_2(test_data) == 45000

aocd.submit(part_2(input_data), part=2)