test_input = """199
200
208
210
200
207
240
269
260
263"""

test_data = [int(n) for n in test_input.splitlines()]


def n_greater(data):
    n = sum(1 for i in range(1, len(test_data)) if test_data[i] > test_data[i - 1])
    return n


assert n_greater(test_data) == 7

with open("input_1.txt", "r") as fp:
    data = [int(line) for line in fp.readlines()]

print("1st part:", n_greater(data))


def sums_greater(data):
    s = 0
    # If list has 10 elements, 'i' has to go from 1 to 7
    for i in range(1, len(data) - 2):
        if sum(data[i : i + 3]) > sum(data[i - 1 : i + 2]):
            s += 1

    return s


assert sums_greater(test_data) == 5

print("2nd part:", sums_greater(data))
