from utils import * 

example_input = """A Y
B X
C Z
"""


def read_data(s):
    return [line.replace(' ', '') for line in s.splitlines()]


example_data = read_data(example_input)
assert len(example_data) == 3
assert example_data[0] == "AY"

def score_hands(round):
    match round:
        case "AX" | "BY" | "CZ":
            return 3
        case "AY" | "BZ" | "CX":
            return 6
        case _:
            return 0


# Part 1
def part1(data):
    """ rock = 1 point, paper = 2 pts, scissor = 3 pts
    loss = 0 pt, draw = 3, win = 6 pts
    """
    return sum((score_hands(hand) + ord(hand[1]) - ord("W")) for hand in data)


assert part1(example_data) == 15

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

def score_2(line):
    opp, result = line[0], line[1]
    base = ord("A")
    if result == "Y":
        return 3 + ord(opp) - base + 1
    elif result == "X":
        return (ord(opp) - base - 1)%3 + 1
    else:
        return 6 + (ord(opp) - base + 1)%3 + 1

# Part 2
def part2(data):
    """X = 0, Y = 3, Z = 6
    X = loss, play = opponent - 1 (modulo, etc)
    Y = draw, play like the opponent
    Z = win, play = opponent + 1
    """
    return sum(score_2(h) for h in data)


assert part2(example_data) == 12

aocd.submit(part2(puzzle_data))
