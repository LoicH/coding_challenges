from utils import *


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
    # Even permutations, (x,y,z), (y,z,x), (z,x,y)
    evens = ([0, 1, 2], [1, 2, 0], [2, 0, 1])
    # Odd permutations, (y,x,z), (z,y,x), (x,z,y)
    odds = ([0, 2, 1], [1, 0, 2], [2, 1, 0])
    even_signs = [(1, 1, 1), (-1, -1, 1), (-1, 1, -1), (1, -1, -1)]
    odd_signs = (np.array(even_signs) * -1).tolist()
    for cols, sign in itertools.product(evens, even_signs):
        yield a[:, cols] * sign
    for cols, sign in itertools.product(odds, odd_signs):
        yield a[:, cols] * sign


def array_to_set(beacons):
    return set(tuple(b) for b in beacons)


@lru_cache
def intra_scanner_distances(i, beacons_byte: bytes):
    print("Computing intra scanner distances for scanner ", i)
    beacons = np.frombuffer(beacons_byte, dtype=np.int32).reshape(-1, 3)
    combinations = itertools.combinations(np.array(beacons), r=2)
    return set(np.linalg.norm(a - b) for (a, b) in combinations)


def match_scanners(si, rotations_j, i, j):
    # First check the intra-scanner distances
    # If they don't even match, don't bother trying to align these two scanners
    dists_i = intra_scanner_distances(i, si.tostring())
    dists_j = intra_scanner_distances(j, rotations_j[0].tostring())
    if len(dists_i & dists_j) < 12:
        print("Scanners {} and {} can't match, skipping.".format(i, j))
        return None, None
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
            shifted_beacons_j = beacons_j + sj_pos
            # What does the scanner 'i' see?
            # What are the beacons that match?
            matched_beacons = beacons_i.intersection(array_to_set(shifted_beacons_j))
            if len(matched_beacons) >= 12:
                return sj_pos, shifted_beacons_j
    return None, None


# Both parts
def solve(scanners):
    scanners_positions = {0: np.zeros(3)}
    fixed_scanners = {0}
    unique_beacons = set()
    scanner_to_fix = set(range(1, len(scanners)))
    # Pre-computing all the rotations se we don't have to do it all the time
    all_rotations = [[]]
    # could speed up the computation by computing the intra-scanner distances,
    # and check the similarity between scanners
    for s in tqdm(scanner_to_fix, desc="Pre-computing the rotations"):
        all_rotations.append(list(rotations24(scanners[s])))
    while len(scanner_to_fix) > 0:
        i = fixed_scanners.pop()
        for j in tqdm(
            scanner_to_fix,
            desc="Matching scanner {} with {} scanners".format(i, len(scanner_to_fix)),
        ):
            si, sj = scanners[i], all_rotations[j]
            pos_j, beacons_j = match_scanners(si, sj, i, j)
            if pos_j is not None:
                print("Matched scanners {} and {}\n".format(i, j))
                scanners[j] = beacons_j
                scanners_positions[j] = pos_j
                unique_beacons |= array_to_set(si) | array_to_set(beacons_j)
                fixed_scanners.add(j)
        scanner_to_fix = scanner_to_fix - fixed_scanners
    inter_distances = [
        int(np.abs(a - b).sum())
        for a, b in itertools.combinations(scanners_positions.values(), 2)
    ]

    return len(unique_beacons), max(inter_distances)


ex_p1, ex_p2 = solve(example_data)
assert ex_p1 == 79, "Wrong output for example part 1"
print("Test example accepted for part 1")

puzzle_input = aocd.get_data(day=19)

assert ex_p2 == 3621
print("Test example accepted for part 2")


puzzle_data = read_data(puzzle_input)

puzzle_p1, puzzle_p2 = solve(puzzle_data)

try:
    aocd.submit(puzzle_p1, day=19)
except aocd.AocdError:
    print("Already sent an answer?")
aocd.submit(puzzle_p2, day=19)
