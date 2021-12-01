import re

def part_one(lines, verbose=False):
    memory = {}
    for line in lines:
        if line.startswith("mask = "):
            mask = line.replace("mask = ", '')
            ones = int(mask.replace('X', '0'), 2)
            zeros = int(mask.replace('X', '1'), 2)
            if verbose:
                print("Mask : ")
                print("Ones  : {}".format(bin(ones)))
                print("Zeros : {}".format(bin(zeros)))
        elif line.startswith("mem"):
            mem_pattern = r"mem\[(\d+)\] = (\d+)"
            match = re.match(mem_pattern, line)
            if not match and verbose:
                print("No match for line {}".format(line))
            address = match.group(1)
            value = int(match.group(2))
            new_val = (value | ones) & zeros
            if verbose:
                print("Old val: {} ({})".format(bin(value), value))
                print("New val: {} ({})".format(bin(new_val), new_val))
            memory[address] = new_val
    return sum(memory.values())

def part_two(lines, verbose=False):
    memory = {}
    for line in lines:
        if line.startswith("mask = "):
            mask = line.replace("mask = ", '')
            ones = int(mask.replace('X', '0'), 2)
            zeros = int(mask.replace('X', '1'), 2)
            x_pos = [i for (i,c) in enumerate(mask) if c == 'X']
            if verbose:
                print("Mask :  {}".format(mask))
                print("Ones  : {}".format(bin(ones)))
                print("Zeros : {}".format(bin(zeros)))
        elif line.startswith("mem"):
            mem_pattern = r"mem\[(\d+)\] = (\d+)"
            match = re.match(mem_pattern, line)
            if not match and verbose:
                print("No match for line {}".format(line))
            address = int(match.group(1))
            value = int(match.group(2))
            if verbose:
                print("Input: Assign {} to address {}".format(value, address))
            formatting = '036b'
            new_address = (address | ones)# & zeros
            new_add_pattern = format(new_address, formatting)
            if verbose:
                print("New address: {} ({})".format(new_add_pattern, new_address))
            
            for n in range(2**(len(x_pos))):
                b = format(n, '0{}b'.format(len(x_pos)))
                final_address = new_add_pattern
                for r, x in enumerate(x_pos):
                    final_address = final_address[:x]+b[r]+final_address[x+1:]
                int_final_address = int(final_address, 2)
                if verbose:
                    print("Final address: {} ({})".format(final_address, int_final_address))
                memory[int_final_address] = value

            
    return sum(memory.values())


def parse_input(path):
    with open(path) as f_in:
        input_lines = [line.rstrip() for line in  f_in.readlines() if len(line.rstrip()) > 0]
    return input_lines

if __name__ == "__main__":
    
    input_example = parse_input("input_example.txt")
    assert(part_one(input_example, verbose=False) == 165), "Wrong output for part one"

    input_example_two = parse_input("input_example_2.txt")
    assert(part_two(input_example_two, verbose=True) == 208), "Wrong output for part two"

    puzzle_input = parse_input("input.txt")
    print("Part one:", part_one(puzzle_input, verbose=False))
    print("Part two:", part_two(puzzle_input, verbose=False))