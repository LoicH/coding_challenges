import aocd
import math

test_input = """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""


def read_data(s):
    return [
        int(line.replace("L", "-").replace("R", "+")) for line in s.strip().split("\n")
    ]


def part_1(ns):
    dial = 50
    zeroes = 0
    for move in ns:
        dial += move
        dial = dial % 100
        if dial == 0:
            zeroes += 1
    return zeroes


test_data = read_data(test_input)
assert part_1(test_data) == 3

with open("input_1.txt") as fp:
    input_data = read_data(fp.read())

print(part_1(input_data))

# Part 2
tests = {
    test_input: 6,
    "L50\nL1": 1,
    "L150\nL1":2,
    "R50\nL1":1,
    "R150\nL1":2
}


def part_2(ns):
    dial = 50
    zeroes = 0
    for move in ns:
        dial += move
        zeroes += abs(dial // 100)
        if dial == move and move < 0:
            zeroes -= 1
        if dial <= 0 and dial%100==0:
            zeroes += 1
        dial %= 100
    return zeroes

for i,(ex,res) in enumerate(tests.items()):
    ex_data = read_data(ex)
    assert part_2(ex_data) == res, f"{i}: {part_2(ex_data)} != {res}"

print(part_2(input_data))
