def read_data(txt: str):
    [raw_ranges, raw_ids] = txt.strip().split("\n\n")
    ranges = []
    for l in raw_ranges.split():
        [a, b] = l.split("-")
        ranges.append(range(int(a), int(b) + 1))
    ids = [int(n) for n in raw_ids.split()]
    return ranges, ids


input_example = read_data(  # Added 9-21 thanks to https://redd.it/1peo1b8
    """3-5
10-14
16-20
12-18
9-21

1
5
8
11
17
32
"""
)


def is_fresh(n, ranges):
    return any(n in r for r in ranges)


def part1(inp) -> int:
    ranges, ids = inp
    return sum(is_fresh(n, ranges) for n in ids)


# assert part1(input_example) == 3, "Error: part 1 on example"

input_data = read_data(open("input_5.txt").read())
# print(part1(input_data))


def part2(inp):
    ranges, _ = inp
    new_ranges = []
    # We'll iterate over ranges and for every range 'r':
    # 0. Every previously seen range that is inside this new range 'r' shoud disappear, 
    # Then we'll encounter exactly one of the following cases:
    # 1. There's no overlap with any of the previously seen ranges 
    #    => It's a new range
    # 2. The lower bound of 'r' is part of one range A and the upper bound
    #    is part of another range B => replace A and B with range(lower_A, upper_B) 
    #    => Merging two previously seen ranges
    # 3. Only one bound of the current range 'r' is part of a previous range 
    #    => transform this previous range
    # 4. The current range 'r' is fully inside another previous range, don't change anything

    for r in ranges:
        special_cases_debug = [480330678306927, 403027733769497]
        if r.start in special_cases_debug or r[-1] in special_cases_debug:
            # special case to debug
            print(f"Special case! {r=}")
        fully_enclosed_ranges = [
            (i, c) for i, c in enumerate(new_ranges) if c.start in r and c[-1] in r
        ]
        for i, c in fully_enclosed_ranges[::-1]:
            print(f"{c=} is fully enclosed in {r=}, removing index {i}")
            new_ranges.pop(i)
        contain_lower = [(i, a) for i, a in enumerate(new_ranges) if r.start in a]
        assert (
            len(contain_lower) <= 1
        ), f"contain_lower should not contain {len(contain_lower)} elements"
        contain_upper = [(i, b) for i, b in enumerate(new_ranges) if r[-1] in b]
        assert (
            len(contain_upper) <= 1
        ), f"contain_upper should not contain {len(contain_upper)} elements"
        # Check the previously described cases
        if not contain_lower and not contain_upper:  # New range that doesn't overlap
            print(f"First time we see {r}")
            new_ranges.append(r)
        elif contain_lower and contain_upper:  # New range merging two seen ranges
            (i_a, a) = contain_lower[0]
            (i_b, b) = contain_upper[0]
            # If we remove the small index first, there would be issues when removing the bigger index
            print(f"{r} lets {a} merge with {b}")
            new_ranges.pop(max(i_a, i_b))
            new_ranges.pop(min(i_a, i_b))
            new_ranges.append(range(a.start, b[-1] + 1))
        elif contain_lower and not contain_upper:
            (i_a, a) = contain_lower[0]
            print(f"{r} augments {a} towards the right")
            new_ranges[i_a] = range(a.start, r[-1] + 1)
        elif not contain_lower and contain_upper:
            (i_b, b) = contain_upper[0]
            print(f"{r} augments {b} towards the left")
            new_ranges[i_b] = range(r.start, b[-1] + 1)
        else:
            print("What the heck??")

    return sum(len(r) for r in new_ranges)


part_2_example_res = part2(input_example)
assert part_2_example_res == 16, f"Error: part 2 on example (res={part_2_example_res})"
# Because I added the '9-21' range this is not the input example

print(part2(input_data))
