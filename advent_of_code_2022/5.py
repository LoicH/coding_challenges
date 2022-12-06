from utils import * 

example_input = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""


def read_data(s):
    stack_input, move_input = [elt.splitlines() for elt in s.split('\n\n')]
    # parse stacks, the first stack is stacks[0]
    stacks = defaultdict(list)
    for line in tqdm(stack_input, desc="Parsing stacks"):
        chars = line[1::4]
        if chars.startswith('123'): # stop parsing stacks
            break
        for i, c in enumerate(chars):
            if c != ' ':
                stacks[i].append(c)
    stacks = [list(reversed(stacks[i])) for i in range(len(stacks))]

    # parse moves
    moves = [] # contains (n, from, to)
    for l in tqdm(move_input, desc="Parsing moves"):
        pre_length = 5
        FROM = " from "
        TO = " to "
        mid_idx = l.index(FROM)
        end_idx = l.index(TO)
        n = int(l[pre_length:mid_idx])
        f = int(l[mid_idx+len(FROM):end_idx])
        to = int(l[end_idx+len(TO):])
        moves.append((n, f-1, to-1))

    return stacks, moves

example_data = read_data(example_input)
assert len(example_data) == 2

# Part 1
def part1(data):
    stacks, moves = data
    stacks = stacks.copy()
    for n, f, to in tqdm(moves, desc="Moving, part 1"):
        stacks[to].extend(list(reversed(stacks[f][-n:])))
        del stacks[f][-n:]
    return ''.join(stacks[i].pop() for i in range(len(stacks)))



assert part1(example_data) == "CMZ"


with open("aoc_2022_day05_large_input.txt", "r") as fp:
    puzzle_input = fp.read()

puzzle_data = read_data(puzzle_input)

print(f"Part 1: {part1(puzzle_data)}")

# try:
#     aocd.submit(part1(puzzle_data))
# except aocd.AocdError:
#     print("Already sent an answer?")

# Part 2
def part2(data):
    stacks, moves = data
    for n, f, to in moves:
        stack_src = stacks[f]
        remain = stack_src[:len(stack_src)-n]
        stack_to_move = stack_src[-n:]
        new_stack = stacks[to] + stack_to_move
        stacks[f] = remain
        stacks[to] = new_stack
    return ''.join(stacks[i].pop() for i in range(len(stacks)))



assert part2(example_data) == "MCD"

aocd.submit(part2(puzzle_data))
