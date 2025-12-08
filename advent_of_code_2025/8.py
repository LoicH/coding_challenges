from functools import reduce
import operator

from tqdm import tqdm


def read_data(txt: str):
    return [tuple(int(n) for n in l.split(',')) for l in txt.strip().split()]


raw_input_example = """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""

def distance(a, b) -> int:
    return sum((a_i - b_i)**2 for (a_i, b_i) in zip(a,b) )

def part1(data, connections=1000, verbose=False) -> int:
    distances = [] # List of (dist, (idx_a, idx_b))
    if verbose:
        print("Computing distances...")
    for i, a in enumerate(data):
        for j, b in enumerate(data[i+1:]):
            distances.append((distance(a,b), (i,j+i+1)))
    distances = sorted(distances, key=lambda t: t[0])
    boxes_to_circuits = {} # dict of box -> circuit index
    circuits = {} # dict of circuit idx -> list of boxes
    for i, (_, (idx_a, idx_b)) in enumerate(distances[:connections]):
        a, b = data[idx_a], data[idx_b]
        if verbose:
            print(f"Connection {i+1}/{connections}...")
            print(f"Closest boxes are {a=} and {b=}")
        # if a and b are not in a circuit, create a new one
        if a not in boxes_to_circuits and b not in boxes_to_circuits:
            new_circuit_id = len(circuits)
            circuits[new_circuit_id] = [a,b]
            boxes_to_circuits[a] = new_circuit_id
            boxes_to_circuits[b] = new_circuit_id
            if verbose:
                print(f"Adding them to a new circuit with id = {new_circuit_id}")
        # if a and b are in different circuits, merge them all in a's circuit
        elif a in boxes_to_circuits and b in boxes_to_circuits:
            a_circuit = boxes_to_circuits[a]
            b_circuit = boxes_to_circuits[b]
            if a_circuit == b_circuit:
                if verbose:
                    print("They were in the same circuit! Move on")
                continue
            if verbose:
                print(f"Merge all {len(circuits[b_circuit])} boxes of B's circuit inside A's")
            for bs_circuits_box in circuits[b_circuit]:
                boxes_to_circuits[bs_circuits_box] = a_circuit
            circuits[a_circuit] += circuits[b_circuit]
            circuits[b_circuit] = []
            if verbose:
                print(f"Now we have {len(circuits)} different circuits")
        # if a is in a circuit, add b to this circuit
        elif a in boxes_to_circuits and b not in boxes_to_circuits:
            a_circuit = boxes_to_circuits[a]
            if verbose:
                print(f"Add B to circuit #{a_circuit}")
            boxes_to_circuits[b] = a_circuit
            circuits[a_circuit] += [b]
        # if b is in a circuit, add a to this circuit
        elif a not in boxes_to_circuits and b in boxes_to_circuits:
            b_circuit = boxes_to_circuits[b]
            if verbose:
                print(f"Add A to circuit #{b_circuit}")
            boxes_to_circuits[a] = b_circuit
            circuits[b_circuit] += [a]
        else:
            print("What the heck??")
    circuits_sizes = sorted([len(c) for c in circuits.values()])[-3:]
    product = reduce(operator.mul, circuits_sizes)
    if verbose:
        print(f"Top 3 biggest circuits sizes: {circuits_sizes}, {product=}")
    return product


input_example = read_data(raw_input_example)
part1_res = part1(input_example, connections=10, verbose=True)
assert part1_res == 40, f"Error: part 1 on example {part1_res=}"

input_data = read_data(open("input_8.txt").read())
print(f"Result on puzzle input: {part1(input_data, connections=1000, verbose=True)}")


def part2(data, verbose=False):
    # TODO: Same logic as part 1, except don't stop at 10 or 1000 connections
    # Use circuits.pop() when merging circuits
    # When creating a new circuit, find the smallest free integer 
    # (len(circuits) risks overwriting existing circuits if we free other circuits)
    distances = [] # List of (dist, (idx_a, idx_b))
    if verbose:
        print("Computing distances...")
    for i, a in tqdm(enumerate(data)):
        for j, b in enumerate(data[i+1:]):
            distances.append((distance(a,b), (i,j+i+1)))
    distances = sorted(distances, key=lambda t: t[0])
    boxes_to_circuits = {} # dict of box -> circuit index
    circuits = {} # dict of circuit idx -> list of boxes
    x_coord_product = None
    for i, (_, (idx_a, idx_b)) in tqdm(enumerate(distances)):
        a, b = data[idx_a], data[idx_b]
        if verbose:
            print(f"Connection {i+1}/{len(data)}...")
            print(f"Closest boxes are {a=} and {b=}")
        # if a and b are not in a circuit, create a new one
        if a not in boxes_to_circuits and b not in boxes_to_circuits:
            new_circuit_id = len(circuits)
            circuits[new_circuit_id] = [a,b]
            boxes_to_circuits[a] = new_circuit_id
            boxes_to_circuits[b] = new_circuit_id
            if verbose:
                print(f"Adding them to a new circuit with id = {new_circuit_id}")
            x_coord_product = a[0] * b[0]
        # if a and b are in different circuits, merge them all in a's circuit
        elif a in boxes_to_circuits and b in boxes_to_circuits:
            a_circuit = boxes_to_circuits[a]
            b_circuit = boxes_to_circuits[b]
            if a_circuit == b_circuit:
                if verbose:
                    print("They were in the same circuit! Move on")
                continue
            if verbose:
                print(f"Merge all {len(circuits[b_circuit])} boxes of B's circuit inside A's")
            for bs_circuits_box in circuits[b_circuit]:
                boxes_to_circuits[bs_circuits_box] = a_circuit
            circuits[a_circuit] += circuits[b_circuit]
            circuits[b_circuit] = []
            if verbose:
                print(f"Now we have {len(circuits)} different circuits")
            x_coord_product = a[0] * b[0]
        # if a is in a circuit, add b to this circuit
        elif a in boxes_to_circuits and b not in boxes_to_circuits:
            x_coord_product = a[0] * b[0]
            a_circuit = boxes_to_circuits[a]
            if verbose:
                print(f"Add B to circuit #{a_circuit}")
            boxes_to_circuits[b] = a_circuit
            circuits[a_circuit] += [b]
        # if b is in a circuit, add a to this circuit
        elif a not in boxes_to_circuits and b in boxes_to_circuits:
            x_coord_product = a[0] * b[0]
            b_circuit = boxes_to_circuits[b]
            if verbose:
                print(f"Add A to circuit #{b_circuit}")
            boxes_to_circuits[a] = b_circuit
            circuits[b_circuit] += [a]
        else:
            print("What the heck??")
    # Return the product of X coordinates of the last two boxes
    return x_coord_product

test_cases_part_2 = {
    raw_input_example: 25272,
}

print("Computing test cases for part 2...")
for i, (test_input, expected) in enumerate(test_cases_part_2.items()):
    print(f"Test case #{i+1}...")
    computed = part2(read_data(test_input), verbose=True)
    assert computed == expected, f"Failed test #{i+1}"

print("Tests done, computing part 2 on full puzzle input...")
print(part2(input_data, verbose=True))
