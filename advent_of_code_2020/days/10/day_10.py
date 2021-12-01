
def part_one(sorted_lines, verbose=False):
    differences = {1:0, 3:1}

    sorted_lines = [0] + sorted_lines
    if verbose:
        print("Sorted: {}".format(sorted_lines))
    for a,b in zip(sorted_lines[:-1], sorted_lines[1:]):
        if b-a in differences:
            if verbose:
                print("{} -> {}".format(a, b))
            differences[b-a] += 1
    if verbose:
        print("Differences: {}".format(differences))
    product = 1
    for v in differences.values():
        product *= v
    return product
    
def find_mult_factor(n):
    consec_mult = {1: 1, 2: 1, 3: 2, 4: 4, 5: 7, 6: 13}
    return consec_mult[n]

def part_two(sorted_lines, verbose=False):
    # Find sequence of consecutive numbers 
    lines = [0] + sorted_lines
    i = 0
    possibilites = 1
    while i < len(lines):
        l = 1
        while i+l < len(lines) and lines[i+l] == lines[i+l-1] +1:
            l += 1
        if verbose:
            print("Found a sequence of {}: {}".format(l, lines[i:i+l]))
        possibilites *= find_mult_factor(l)
        i += l
    return possibilites


if __name__ == "__main__":
    with open("input_example_day_10.txt") as f_in_ex:
        input_example = sorted([int(s) for s in f_in_ex.readlines()])
    
    print(input_example)
    assert(part_one(input_example, verbose=True) == 220), "Wrong output for part one"
    assert(part_two(input_example, verbose=False) == 19208), "Wrong output for part two"

    with open("input_day_10.txt") as f_input:
        puzzle_input = sorted([int(s) for s in f_input.readlines()])
    
    
    print("Part one:", part_one(puzzle_input, verbose=True))
    print("Part two:", part_two(puzzle_input, verbose=True))