input_ex = """939
7,13,x,x,59,x,31,19"""

input_puzzle = """1006726
23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,647,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,x,x,x,x,x,x,29,x,557,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17"""


def parse_input(txt):
    lines = txt.split('\n')
    return lines
    
def part_one(lines):
    arrival = int(lines[0])
    d = {int(s):-arrival%int(s) for s in lines[1].split(',') if s != 'x'}
    m = min(d, key=d.get)
    return m*d[m]

def part_two(lines, v=False):
    constraints = {int(s):i for (i, s) in enumerate(lines[1].split(',')) if s != 'x'}
    print("constraints:", constraints)
    k = 0
    success = False
    m = max(constraints.keys())
    while not success:
        k += 1
        t = m*k - constraints[m]
        if v:
            print(k, t)
        divisions = [((t+delay)%bus) == 0 for (bus, delay) in constraints.items()]
        if v:
            print(divisions)
        success = all(divisions)
    # Or use "Chinese Remainder Theorem"...
    return t

input_lines = parse_input(input_ex)
assert(part_one(input_lines) == 295), "Wrong output for part one"
assert(part_two(input_lines, v=True) == 1068781), "Wrong output for part two"
puzzle_lines = parse_input(input_puzzle)

print("Part one:", part_one(puzzle_lines))
print("Part two:", part_two(puzzle_lines, v=False))