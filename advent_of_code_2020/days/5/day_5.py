def seat_id(seat):
    binary = seat.replace("R", "1").replace("L", "0").replace("B", "1").replace("F", "0")
    return int(binary, 2)

def part_one(seats, verbose=False):
    return max(seat_id(s) for s in seats)

def part_two(seats, verbose=False):
    ids = sorted([seat_id(s) for s in seats])
    m = ids[0]
    for i, s in enumerate(ids):
        if m+i != s:
            return m+i

example_ids = {"FBFBBFFRLR": 357,
    "BFFFBBFRRR": 567, 
    "FFFBBBFRRR": 119, 
    "BBFFBBFRLL": 820, 
}

if __name__ == "__main__":
    for seat, i in example_ids.items():
        assert(seat_id(seat) == i), "Wrong seat id for {}, got {} instead of {}".format(seat, seat_id(seat), i)

    with open("input_day_5.txt") as f_in:
        seats = f_in.readlines()
    print("Part one:", part_one(seats))
    print("Part two:", part_two(seats))

