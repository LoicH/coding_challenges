def run_program(lines):
    lines_seen = set()
    acc = 0
    i = 0
    while i not in lines_seen and i<len(lines):
        instr, arg = lines[i].split(' ')
        lines_seen.add(i)
        if instr == "acc":
            acc += int(arg)
            i += 1
        elif instr == "jmp":
            i += int(arg)
        else:
            i += 1
    return i==len(lines), acc

def part_one(lines, verbose=False):
    _, acc = run_program(lines)
    return acc
    
def part_two(lines, verbose=False):
    # Transforming all "jmp" to "nop"
    for (i, line) in enumerate(lines):
        if line.startswith("jmp"):
            new_program = lines.copy()
            new_program[i] = line.replace("jmp", "nop")
            if verbose:
                print("Changing line #{}".format(i))
            terminated, acc = run_program(new_program)
            if terminated:
                if verbose:
                    print("Terminated!")
                return acc
            if verbose:
                print("The search continues")
    # Transforming all "nop" to "jmp"
    # Don't even have to do this

if __name__ == "__main__":
    with open("input_example_day_8.txt") as f_in_ex:
        input_example = f_in_ex.readlines()
    
    assert(part_one(input_example, verbose=False) == 5), "Wrong output for part one"
    assert(part_two(input_example, verbose=False) == 8), "Wrong output for part two"

    with open("input_day_8.txt") as f_input:
        puzzle_input = f_input.readlines()
    print("Part one:", part_one(puzzle_input))
    print("Part two:", part_two(puzzle_input))