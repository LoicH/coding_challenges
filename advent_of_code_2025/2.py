from typing import List
import aocd
import math

from aocd.models import AOCD_DATA_DIR 

test_input = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124"""

def read_data(s:str):
    str_ranges = s.replace('\n', '').split(',')
    int_ranges = []
    for r in str_ranges:
        [a,b] = r.split('-')
        int_ranges.append(range(int(a),int(b)+1))
    return int_ranges


def find_invalid_ids(r:range) -> List[int]:
    invalids = []
    for n in r:
        n_digits = math.ceil(math.log10(n))
        if n % (10**(n_digits/2)+1) == 0:
            invalids.append(n)
    return invalids

def part_1(ranges):
    invalid_ids = []
    for r in ranges:
        current_invalids = find_invalid_ids(r)
        print(f"Range: {r}, {current_invalids=}")
        invalid_ids += current_invalids
    return sum(invalid_ids)

test_data = read_data(test_input)
# assert part_1(test_data) == 1227775554, "Error on test input for part 1"

with open("input_2.txt") as fp:
    input_data = read_data(fp.read())

# print((part_1(input_data)))

def divisorGenerator(n):
    yield (n,1)
    for i in range(2, int(math.sqrt(n)) + 1):
        if n%i==0:
            yield (i, n//i)
            if i*i != n: # We're not in a square root situation
                yield (n//i, i)

def is_invalid_part_2(n:int) -> bool:
    """
    12341234 has 8 digits and 1 0001 divides it (10^4+1) 
    WARNING : we need to check the number of digits, not just the divisibility 
    (1001 is divisible by 1001 but is valid)
    123123123 has 9 digits and 1 001 001 divides it ()
    1212121212 has 10 digits and 101010101 divides it 
    1111111 has 7 digits and 1111111 divides it
    invalids with 2 digits need to be divisible by 11
    invalids with 3 digits need to be divisible by 111
    with 4 digits : 101 (1212 for instance) or 1111
    5 digits : 11111 or ... ? 
    When the number N of digits is prime : only divisible by 1...1
    6 digits : 111 111 (6x1), or 10101 (3x2) or 1001 (2x3)
    """
    n_digits = math.ceil(math.log10(n))
    for (a,b) in divisorGenerator(n_digits):
        # Construct a divisor made of 'a' times '00..01' (with 'b' digits) 
        divisor = 1 
        for _ in range(a-1):
            divisor *= 10**b
            divisor += 1 
        if n%divisor==0:
            return True
    return False 

print("Tests for part 2")
test_invalids = [12341234, 123123123, 1212121212, 1111111, ]
for n in test_invalids:
    assert is_invalid_part_2(n), f"{n} should be detected as invalid"
test_valids = [11211,]
for n in test_valids:
    assert not is_invalid_part_2(n), f"{n} should be valid"

def find_invalid_ids_2(r:range) -> List[int]:
    invalids = []
    for n in r:
        if is_invalid_part_2(n):
            invalids.append(n)
    return invalids

def part_2(ranges):
    invalid_ids = []
    for r in ranges:
        current_invalids = find_invalid_ids_2(r)
        print(f"Range: {r}, {current_invalids=}")
        invalid_ids += current_invalids
    return sum(invalid_ids)


assert part_2([range(11,22)]) == 11
assert part_2(test_data) == 4174379265, "Error on example input for part 2"

print("Tests passed")
print("Computing final answer...")
print(part_2(input_data))
# 28915664443 too high 