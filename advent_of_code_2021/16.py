import itertools
import math
import re
from collections import Counter, defaultdict

import aocd
import numpy as np
from tqdm import tqdm, trange

examples = {
    "D2FE28": 6,
    "38006F45291200": 1 + 6 + 2,
    "EE00D40C823060": 7 + 2 + 4 + 1,
    "8A004A801A8002F478": 16,
    "620080001611562C8802118E34": 12,
    "C0015000016115A2E0802F182340": 23,
    "A0016C880162017C3686B18A3D4780": 31,
}

# Type IDs:
# 4 = literal value, single binary number
# Else = operator packet

# Literal values:
# VVV TTT(=100) A(=1)AAAA B(=1)BBBB C(=0)CCCC 00...00


# For operator packets:
# Length type ID
# 0 -> next 15 bits indicate the length in bits of the sub packets
# 1 -> next 11 bits represent the number of sub packets


def read_data(s):
    b = bin(int(s, 16))[2:]
    b = b.zfill(math.ceil(len(b) / 4) * 4)
    return [int(c) for c in b]


def bits_to_int(bits):
    val = 0
    for b in bits:
        val = (val << 1) + b
    return val


def length_literal(bits):
    """
    - bits: the bits where groups start.

    - returns: the index of the next value. Will be greater than (or equal to) 5
        E.g.: if `bits` is 10111 11110 00101 000, this will output 19
    """
    i = 0
    # We discard all blocks of 5 bits starting with '1'
    while bits[i] == 1:
        i += 5
    # We reached the last block, starting with '0'
    i += 5
    # If there is padding, we reached the end
    if not 1 in bits[i:]:
        return len(bits)
    return i


N_BITS = "bits"
N_PACKETS = "pack"

# Part 1
def part1(bits):
    versions = []
    i = 0
    stack = []
    while i < len(bits):
        v = bits_to_int(bits[i : i + 3])
        versions.append(v)
        i += 3
        type_id = bits_to_int(bits[i : i + 3])
        i += 3
        if type_id == 4:
            # Finish reading this literal
            length = length_literal(bits[i:])
            i += length
            if i == len(bits):
                return sum(versions)
            (t, n) = stack.pop()
            if t == N_BITS:
                n -= length
                n -= 6
            else:
                n -= 1
            if n > 0:
                stack.append((t, n))
            elif len(stack) == 0:
                return sum(versions)
        else:
            length_type = bits[i]
            i += 1
            if length_type == 0:
                # The next 15 bits contain the number of bits
                n_bits = bits_to_int(bits[i : i + 15])
                stack.append((N_BITS, n_bits))
                i += 15
            else:
                # The next 11 bits represent the number of packets
                n_packets = bits_to_int(bits[i : i + 11])
                stack.append((N_PACKETS, n_packets))
                i += 11


for i, (ex_input, expect_val) in enumerate(examples.items()):
    print("Testing on example {}".format(ex_input))
    val = part1(read_data(ex_input))
    assert (
        val == expect_val
    ), "Wrong output for example #{}: '{}', got {} instead of {}".format(
        i + 1, ex_input, val, expect_val
    )
    print("Success!")


puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)
aocd.submit(part1(puzzle_data))

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data))
