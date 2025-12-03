from typing import List
import numpy as np

input_example = """987654321111111
811111111111119
234234234234278
818181911112111
"""


def read_data(s:str) -> List[List[int]]:
    return [[int(c) for c in line] for line in s.split()]

def max_joltage1(bank):
    first_idx = np.argmax(bank[:-1])
    last_digit = max(bank[first_idx+1:])
    return bank[first_idx]*10+last_digit

def part1(ratings):
    return sum(max_joltage1(b) for b in ratings)

data_example = read_data(input_example)
assert part1(data_example)==357, "Error: part 1 on example should be 357"

with open("input_3.txt") as fp:
    input_data = read_data(fp.read())

# print(part1(input_data))

def max_joltage2(bank):
    """(23)4(2)34234234278 (15 digits)
    find the argmax of bank[:-11]: 2 (position of 4)
    new_bank = bank[2 (current_argmax) +1:]
    find the argmax of new_bank[-10]: 1 (position of 3)
    etc
    """
    full_number = 0
    # Find the first 11 digits:
    for i in range(11,0,-1):
        idx = np.argmax(bank[:-i])
        full_number = full_number*10+bank[idx]
        bank = bank[idx+1:]
    # Find the last digit:
    return full_number * 10 + max(bank)

def part2(ratings):
    s = 0
    for b in ratings:
        print(''.join(str(n) for n in b), end=' ')
        j = max_joltage2(b)
        print(j)
        s += j
    return s


assert part2(data_example)==3121910778619, "Error: part 2 on example"

print(part2(input_data))