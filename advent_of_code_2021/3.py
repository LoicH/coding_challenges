test_input = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

test_data = [line for line in test_input.splitlines()]


def bits_to_int(bits):
    return int(sum(b * 2 ** i for i, b in enumerate(bits[::-1])))


# Part 1
def part1(data):
    n_lines = len(data)
    numbers = [[int(b) for b in line.rstrip()] for line in data]
    n_bits = len(numbers[0])
    each_bit_sum = [sum(numbers[i][j] for i in range(n_lines)) for j in range(n_bits)]
    gamma_bits = [int(n > n_lines / 2) for n in each_bit_sum]
    gamma = bits_to_int(gamma_bits)
    epsilon = int(2 ** n_bits - 1) - gamma
    return epsilon * gamma


assert part1(test_data) == 198

with open("input_3.txt", "r") as fp:
    data = [line for line in fp.readlines()]

print("1st part:", part1(data))


def partition_numbers(numbers, pos):
    partition = {0: [], 1: []}
    one_count = 0
    for bits in numbers:
        one_count += bits[pos]
        partition[bits[pos]].append(bits)
    if one_count >= len(numbers) / 2:
        return partition[1], partition[0]
    else:
        return partition[0], partition[1]


# Part 2
def part2(data):
    n_lines = len(data)
    numbers = [[int(b) for b in line.rstrip()] for line in data]
    n_bits = len(numbers[0])
    dominant, minority = partition_numbers(numbers, 0)
    pos = 1
    while len(dominant) > 1:
        dominant = partition_numbers(dominant, pos)[0]
        pos += 1
    oxygen = bits_to_int(dominant[0])
    pos = 1
    while len(minority) > 1:
        minority = partition_numbers(minority, pos)[1]
        pos += 1

    co2 = bits_to_int(minority[0])
    return oxygen * co2


assert part2(test_data) == 230
print("2nd part:", part2(data))
