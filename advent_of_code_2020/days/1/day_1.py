SUM = 2020
def part_one(numbers, total=SUM):
    for i, n in enumerate(numbers):
        for m in numbers[i+1:]:
            if n+m == total:
                return n*m
    return None

def part_two(numbers, total=SUM):
    for i, n in enumerate(numbers):
        res = part_one(numbers[i+1:], total=SUM-n)
        if res:
            return n*res

if __name__ == "__main__":
    input_example = """1721
    979
    366
    299
    675
    1456"""

    example_nb = [int(n) for n in input_example.split("\n")]

    assert(part_one(example_nb) == 514579)
    assert(part_two(example_nb) == 241861950)

    with open("input_day_1.txt") as f:
        input_nb = f.read()
    numbers = [int(n) for n in input_nb.split("\n")]

    print("Part one:", part_one(numbers))
    print("Part two:", part_two(numbers))

