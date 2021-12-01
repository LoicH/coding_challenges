
def part_one(txt, slope=(3,1), verbose=False):
    trees_seen = 0
    lines = txt.split('\n')
    width = len(lines[0])
    i, j = 0, 0
    while i<len(lines):
        if lines[i][j] == '#':
            trees_seen += 1
        i += slope[1]
        j = (j+slope[0]) % width
    return trees_seen
    
def part_two(txt, slopes=[(1,1), (3,1), (5,1), (7,1), (1,2)], verbose=False):
    product = 1
    for s in slopes:
        p = part_one(txt, s, verbose=False)
        product *= p
        if verbose:
            print("After slope {}, saw {} trees, product is now {}".format(s, p, product))
    return product

if __name__ == "__main__":
    with open("input_example_day_3.txt") as f:
        example = f.read()

    assert(part_one(example, verbose=False) == 7)
    assert(part_two(example, verbose=False) == 336)
    
    # computing the input example
    with open("input_day_3.txt") as f:
        input_day = f.read()
    print("Part one:", part_one(input_day, verbose=False))
    print("Part two:", part_two(input_day, verbose=False))
