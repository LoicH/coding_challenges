from utils import *


class Diagram:
    def __init__(self, s: str):
        self.diagram = [line[1:-1] for line in s.splitlines()[1:-1]]
        self.width = len(self.diagram[0])
        self.height = len(self.diagram)
        self.letters = "ABCD"

    def get_corridor(self) -> str:
        return self.diagram[0]

    def get_column(self, col_nb: int) -> str:
        return [line[self.get_col_idx(col_nb)] for line in self.diagram][1:]

    def get_col_idx(self, col_nb: int):
        return 2 * (col_nb + 1)

    def should_move_in_col(self, col_nb: int):
        col = self.get_column(col_nb)
        col_idx = self.get_col_idx(col_nb)
        letter = self.letters[col_nb]
        max_move = max(i for i, c in enumerate(col) if c != letter)
        return {(col[i], i + 1, col_idx) for i in range(max_move + 1)}

    def well_placed(self):
        return set.union(*[self.should_move_in_col(i) for i in range(4)])

    def possible_destinations(self, i, j):
        c = self.diagram[i][j]
        destinations = set()
        stack = set((i+di, j+dj) for di,dj in (-1,0), (1,0), (0,-1), (0,1))
        while len(stack) > 0:
            (x,y) = stack.pop()
            if 0<=x<self.height and 0 <= y < self.width and self.diagram[x][y] == '.':
                


example_input = """#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"""


def read_data(s):
    return Diagram(s)


example_data = read_data(example_input)

print(example_data.get_corridor())
print(list(example_data.get_column(i) for i in range(4)))
print(example_data.should_move_in_col(0))
print(example_data.well_placed())


def column(diagram, i):
    return list(reversed([l[i] for l in diagram.splitlines()]))[1:-2]


# Part 1
def part1(data):
    return  # TODO


assert part1(example_data) == 12521

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data))
