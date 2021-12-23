import itertools
import math
import re
from collections import Counter, defaultdict
from operator import mul

import aocd
import numpy as np
from tqdm import tqdm, trange
from dataclasses import dataclass


@dataclass
class Expr:
    version: int
    type_id: int
    length: int


@dataclass
class Op(Expr):
    packets: list


@dataclass
class Lit(Expr):
    value: int


examples = {
    "D2FE28": 6,
    "38006F45291200": 1 + 6 + 2,
    "EE00D40C823060": 7 + 2 + 4 + 1,
    "8A004A801A8002F478": 16,
    "620080001611562C8802118E34": 12,
    "C0015000016115A2E0802F182340": 23,
    "A0016C880162017C3686B18A3D4780": 31,
    "0200840080": 0,
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
    b = b.zfill(math.ceil(len(s) * 4))
    return [int(c) for c in b]


def bits_to_int(bits):
    val = 0
    for b in bits:
        val = (val << 1) + b
    return val


def parse_bits(bits) -> Expr:
    version = bits_to_int(bits[0:3])
    type_id = bits_to_int(bits[3:6])
    if type_id == 4:
        # Read a literal value
        i = 6
        value = bits_to_int(bits[i + 1 : i + 5])
        while bits[i] == 1:
            value *= 16
            i += 5
            value += bits_to_int(bits[i + 1 : i + 5])
        lit = Lit(version, type_id, i + 5, value)
        return lit
    bit_id = bits[6]
    if bit_id == 1:
        # read the number of sub packets
        nb_packets = bits_to_int(bits[7:18])
        i = 18
        op = Op(version, type_id, length=18, packets=[])
        for j in range(nb_packets):
            new_pack = parse_bits(bits[i:])
            length = new_pack.length
            i += length
            op.length += length
            op.packets += [new_pack]
        return op
    else:
        # read number of bits for the sub packets
        nb_bits = bits_to_int(bits[7:22])
        i = 22
        op = Op(version, type_id, None, [])
        while nb_bits > 0:
            new_pack = parse_bits(bits[i:])
            length = new_pack.length
            i += length
            nb_bits -= length
            op.packets += [new_pack]
        op.length = i
        return op


def sum_versions(expr: Expr) -> int:
    if isinstance(expr, Lit):
        return expr.version
    else:
        return expr.version + sum(sum_versions(sub_pack) for sub_pack in expr.packets)


def part1(bits):
    expr = parse_bits(bits)
    return sum_versions(expr)


for i, (ex_input, expect_val) in enumerate(examples.items()):
    print("Testing on example {}".format(ex_input))
    val = part1(read_data(ex_input))
    assert (
        val == expect_val
    ), "Wrong output for example #{}: '{}', got {} instead of {}".format(
        i + 1, ex_input, val, expect_val
    )
    print("Success!")


puzzle_input = aocd.get_data(day=16)

puzzle_data = read_data(puzzle_input)
try:
    aocd.submit(part1(puzzle_data), day=16)
except aocd.AocdError:
    print("Already sent an answer?")


operations = {
    0: sum,
    1: math.prod,
    2: min,
    3: max,
    5: (lambda l: int(l[0] > l[1])),
    6: (lambda l: int(l[0] < l[1])),
    7: (lambda l: int(l[0] == l[1])),
}


def value(expr: Expr) -> int:
    if isinstance(expr, Lit):
        return expr.value
    sub_values = [value(p) for p in expr.packets]
    return operations[expr.type_id](sub_values)


# Part 2
def part2(bits):
    expr = parse_bits(bits)
    return value(expr)


# No checks, no brakes
# assert part2(example_data) == 1  # TODO

aocd.submit(part2(puzzle_data), day=16)
