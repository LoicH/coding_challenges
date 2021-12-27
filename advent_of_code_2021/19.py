import itertools
import math
import re
from collections import Counter, defaultdict

import aocd
import numpy as np
from tqdm import tqdm, trange


def read_data(s):
    scanners = [line.rstrip().lstrip().split("\n") for line in s.split("---")[2::2]]

    coords = []
    for scan_listing in scanners:
        beacons = []
        for beacon_line in scan_listing:
            beacons.append([int(c) for c in beacon_line.split(",")])
        coords.append(np.array(beacons))
    return coords


with open("example_input_19.txt", "r") as fp:
    example_input = fp.read()

example_data = read_data(example_input)


def rotations24(a):
    s = set([0, 1, 2])
    for first_sign, first_idx in itertools.product([+1, -1], s):
        for snd_sign, snd_idx in itertools.product([+1, -1], s - {first_idx}):
            third_sign = first_sign * snd_sign
            third_idx = (s - {first_idx, snd_idx}).pop()
            yield a[:, [first_idx, snd_idx, third_idx]] * [
                first_sign,
                snd_sign,
                third_sign,
            ]


def array_to_set(beacons):
    return set(tuple(b) for b in beacons)


def match_scanners(si, rotations_j, i, j):
    # Try to match scanner i and scanner j by choosing a rotation for
    # scanner j and choosing a beacon x in scanner i and a beacon y in scanner j
    # and make them match, and see what other beacons are matching
    beacons_i = array_to_set(si)
    for beacons_j in tqdm(rotations_j, desc="Matching scanners {} and {}".format(i, j)):
        # Setting a rotation for scanner j
        for bx, y in itertools.product(si, range(len(beacons_j))):
            # The new position of scanner j
            # so that beacon x is beacon y
            sj_pos = bx - beacons_j[y]
            # Shift all the beacons relative to the new scanner position
            beacons_j += sj_pos
            # What does the scanner 'i' see?
            # What are the beacons that match?
            matched_beacons = beacons_i.intersection(array_to_set(beacons_j))
            if len(matched_beacons) >= 12:
                return True, beacons_j
    return False, None


# Part 1
def part1(scanners):
    fixed_scanners = {0}
    unique_beacons = set()
    scanner_to_fix = set(range(1, len(scanners)))
    # Pre-computing all the rotations se we don't have to do it all the time
    all_rotations = [[]]
    for s in tqdm(scanner_to_fix, desc="Pre-computing the rotations"):
        all_rotations.append(list(rotations24(scanners[s])))
    while len(scanner_to_fix) > 0:
        i = fixed_scanners.pop()
        for j in tqdm(
            scanner_to_fix,
            desc="Matching scanner {} with {} scanners".format(i, len(scanner_to_fix)),
        ):
            si, sj = scanners[i], all_rotations[j]
            matched, beacons_j = match_scanners(si, sj, i, j)
            if matched:
                print("Matched scanners {} and {}\n".format(i, j))
                scanners[j] = beacons_j
                unique_beacons |= array_to_set(si) | array_to_set(beacons_j)
                fixed_scanners.add(j)
        scanner_to_fix = scanner_to_fix - fixed_scanners
    return len(unique_beacons)


assert part1(example_data) == 79
print("Test example accepted for part 1")

puzzle_input = aocd.get_data(day=19)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data), day=19)
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data), day=19)
