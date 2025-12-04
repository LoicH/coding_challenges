import itertools
from typing import List
import numpy as np
from tqdm import tqdm

def read_data(txt: str) -> List[str]:
    return txt.split()


input_example = read_data(
    """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""
)


def count_neighbours(s: List[str], i: int, j: int):
    """
    i = the index of the line of the roll of paper (0 = first line)
    j = the column of the roll of paper (0 = first column)
    """
    all_pos = itertools.product([-1, 0, 1], repeat=2)
    max_width = len(s[0])
    max_height = len(s)
    neighbs = -1  # We will encounter the roll of paper at position (i,j)
    for di, dj in all_pos:
        if (
            0 <= i + di < max_height
            and 0 <= j + dj < max_width
            and s[i + di][j + dj] == "@"
        ):
            neighbs += 1
    return neighbs


def part1(inp:List[str]) -> int:
    accessibles = 0
    for i in range(len(inp)):
        for j in range(len(inp[0])):
            if inp[i][j] == '@' and count_neighbours(inp,i,j) < 4:
                accessibles += 1
    return accessibles


assert part1(input_example) == 13, "Error: part 1 on example should be 13"

input_data = read_data(open("input_4.txt").read())
print(part1(input_data))


def part2(inp):
    removables = 0
    max_w = len(inp[0])
    max_h = len(inp)
    previous_removed = 0 
    for n in tqdm(range(max_w*max_h)): # worst case: we can remove only one roll at a time
        not_yet_removed = 0
        new_inp = []
        for i in range(len(inp)):
            new_inp.append('')
            for j in range(len(inp[0])):
                if inp[i][j] != '@':
                    new_inp[i] += '.'
                elif inp[i][j] == '@' and count_neighbours(inp,i,j) < 4:
                    removables += 1
                    new_inp[i] += '.'
                else:
                    new_inp[i] += '@'
                    not_yet_removed += 1
        inp = new_inp
        if previous_removed == removables:
            print("Finished")
            break
        print(f"{removables} removed")
        previous_removed = removables
        print(f"{not_yet_removed} rolls remaining")
    return removables


assert part2(input_example) == 43, "Error: part 2 on example"

print(part2(input_data))
