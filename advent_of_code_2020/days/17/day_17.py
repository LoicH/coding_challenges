import itertools

def part_one(init_state, verbose=False):
    def candidates_cubes(active_cubes):
        candidates = active_cubes.copy()
        delta = range(-1, 2)
        for (cube, dx, dy, dz) in itertools.product(active_cubes, delta, delta, delta):
            candidates.add((cube[0]+dx, cube[1]+dy, cube[2]+dz))
        return candidates

    def count_neighbours(cube, active_cubes):
        delta = range(-1, 2)
        count = 0
        for (dx, dy, dz) in itertools.product(delta, delta, delta):
            if 0 == dx == dy == dz:
                continue
            if (cube[0]+dx, cube[1]+dy, cube[2]+dz) in active_cubes:
                count += 1
        return count

    active_cubes = init_state
    for i in range(6):
        candidates = candidates_cubes(active_cubes)
        new_active_cubes = set()
        for cube in candidates:
            neighbours = count_neighbours(cube, active_cubes)
            if neighbours == 3 or (neighbours== 2 and cube in active_cubes):
                new_active_cubes.add(cube)
        if verbose:
            print("Turn {}, {} active cubes.".format(i, len(new_active_cubes)))
        active_cubes = new_active_cubes
    return len(new_active_cubes)

def part_two(init_state, verbose=False):
    def candidates_cubes(active_cubes):
        candidates = active_cubes.copy()
        delta = range(-1, 2)
        for (cube, dx, dy, dz, dw) in itertools.product(active_cubes, delta, delta, delta, delta):
            candidates.add((cube[0]+dx, cube[1]+dy, cube[2]+dz, cube[3]+dw))
        return candidates

    def count_neighbours(cube, active_cubes):
        delta = range(-1, 2)
        count = 0
        for (dx, dy, dz, dw) in itertools.product(delta, delta, delta, delta):
            if 0 == dx == dy == dz == dw:
                continue
            if (cube[0]+dx, cube[1]+dy, cube[2]+dz, cube[3]+dw) in active_cubes:
                count += 1
        return count

    active_cubes = init_state
    for i in range(6):
        candidates = candidates_cubes(active_cubes)
        new_active_cubes = set()
        for cube in candidates:
            neighbours = count_neighbours(cube, active_cubes)
            if neighbours == 3 or (neighbours== 2 and cube in active_cubes):
                new_active_cubes.add(cube)
        if verbose:
            print("Turn {}, {} active cubes.".format(i, len(new_active_cubes)))
        active_cubes = new_active_cubes
    return len(new_active_cubes)

def parse_input(path, verbose=False):
    with open(path) as f:
        input_lines = [line.rstrip() for line in  f.readlines() if len(line.rstrip()) > 0]
    height = len(input_lines)
    width = len(input_lines[0])
    active_cubes = set((x,y,0) for x in range(height) for y in range(width) if input_lines[x][y] == "#")
    if verbose:
        print("Parsed {} active cubes.".format(len(active_cubes)))
    return active_cubes

def parse_input_two(path, verbose=False):
    with open(path) as f:
        input_lines = [line.rstrip() for line in  f.readlines() if len(line.rstrip()) > 0]
    height = len(input_lines)
    width = len(input_lines[0])
    active_cubes = set((x,y,0,0) for x in range(height) for y in range(width) if input_lines[x][y] == "#")
    if verbose:
        print("Parsed {} active cubes.".format(len(active_cubes)))
    return active_cubes

if __name__ == "__main__":
    input_ex = parse_input("input_example.txt", verbose=True)
    assert(part_one(input_ex, verbose=True) == 112), "Wrong output for part one"

    input_puzzle = parse_input("input.txt")
    print("Part one:", part_one(input_puzzle, verbose=False))

    input_ex_two = parse_input_two("input_example.txt", verbose=True)
    assert(part_two(input_ex_two, verbose=True) == 848), "Wrong output for part two"

    input_puzzle_two = parse_input_two("input.txt")
    print("Part two:", part_two(input_puzzle_two, verbose=False))
