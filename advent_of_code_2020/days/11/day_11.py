def occupied_neighbours_part_one(seat_layout, i, j, verbose=False):
    height, width = len(seat_layout), len(seat_layout[0])
    count = 0
    for di in range(-1, 2):
        for dj in range(-1, 2):
            if 0 <= i+di < height and 0 <= j+dj < width and (di != 0 or dj != 0) and seat_layout[i+di][j+dj] == "#":
                count += 1
    return count

def occupied_neighbours_part_two(seat_layout, i, j, verbose=False):
    height, width = len(seat_layout), len(seat_layout[0])
    count = 0
    # (di, dj) is the direction vector
    for di in range(-1, 2):
        for dj in range(-1, 2):
            # Let's move in that direction until we reach the end of the layout, or a seat
            k = 1
            new_i, new_j = i+k*di,j+k*dj
            while 0 <= new_i < height and 0 <= new_j < width and (di != 0 or dj != 0) and seat_layout[new_i][new_j] == ".":
                k += 1
                new_i, new_j = i+k*di,j+k*dj
            # If we're here, either we did not move at all, or we reached the end of the map, or we found
            # a seat
            if 0 <= new_i < height and 0 <= new_j < width and (di != 0 or dj != 0) and seat_layout[new_i][new_j] == "#":
                count += 1
    return count


def next_turn(seat_layout, func_occupied_neighbours, limit_neighbours, verbose=False):
    new_layout = []
    changed = False
    occupied_seats = 0
    for i in range(len(seat_layout)):
        new_layout.append("")
        for j in range(len(seat_layout[i])):
            tile = seat_layout[i][j]
            if tile == '.':
                new_layout[-1] += '.'
            else:
                occ_neighb = func_occupied_neighbours(seat_layout, i, j, verbose=verbose)
                if tile == 'L' and occ_neighb == 0:
                    new_layout[-1] += '#'
                    occupied_seats += 1
                    changed = True
                elif tile == '#' and occ_neighb >= limit_neighbours:
                    new_layout[-1] += 'L'
                    changed = True
                else:
                    new_layout[-1] += tile
                    if tile == '#':
                        occupied_seats += 1
    return changed, new_layout, occupied_seats


def reach_stable_state(seat_layout, func_occ_neighbours, limit_neighbours, verbose=False):
    changed = True
    new_layout = seat_layout
    occupied_seats = None
    while changed:
        if verbose and occupied_seats is not None:
            print("Got {} occupied seats, compute next turn".format(occupied_seats))
        changed, new_layout, occupied_seats = next_turn(new_layout, func_occupied_neighbours=func_occ_neighbours, 
                                                        limit_neighbours=limit_neighbours, verbose=verbose)
    print("Stable state, with {} occupied seats".format(occupied_seats))
    return occupied_seats
    
part_one = lambda l,verbose : reach_stable_state(l, occupied_neighbours_part_one, 4, verbose=verbose)
part_two = lambda l,verbose : reach_stable_state(l, occupied_neighbours_part_two, 5, verbose=verbose)

def parse_input(path):
    with open(path) as f_in:
        input_lines = [line.rstrip() for line in  f_in.readlines() if len(line.rstrip()) > 0]
    return input_lines

if __name__ == "__main__":
    
    input_example = parse_input("input_example.txt")
    assert(part_one(input_example, verbose=False) == 37), "Wrong output for part one"
    assert(part_two(input_example, verbose=True) == 26), "Wrong output for part two"

    puzzle_input = parse_input("input.txt")
    
    print("Part one:", part_one(puzzle_input, verbose=False))
    print("Part two:", part_two(puzzle_input, verbose=False))