from collections import defaultdict
from multiprocessing import Pool

def play_game(numbers, n_turns, verbose=False, n_turns_verbose=1e6):
    memory = defaultdict(lambda : (None, None)) # {n: (last time we said 'n', the time before)}
    for i in range(n_turns):
        # First we iterate over the input
        if i < len(numbers):
            spoken = numbers[i]
            memory[spoken] = (i, None)
        # Then we play
        elif memory[spoken] == (None, None):
            # We never said this number, we should never be here
            if verbose:
                print("Never said this number: {} (mem={})".format(spoken, memory))
        elif memory[spoken][1] is None:
            # We have spoken this number only once 
            spoken = 0
            last, before = memory[spoken]
            memory[spoken] = (i, last)
        else:
            # We have spoken this number more than once
            last, before = memory[spoken]
            spoken = last-before
            last, before = memory[spoken]
            memory[spoken] = (i, last)
        if verbose and (i+1)%n_turns_verbose == 0:
            print("i={:.2e}/{:.1e}, spoken: {}".format(i+1, n_turns, spoken))
    return spoken

part_one = lambda numbers, verbose: play_game(numbers, n_turns=2020, verbose=verbose)
part_two = lambda numbers, verbose: play_game(numbers, n_turns=30000000, verbose=verbose)

def test_example(test_data):
    (numbers, out1, out2) = test_data
    assert(part_one(numbers, verbose=False) == out1), "Wrong output for part_one({}) ".format(numbers)
    print("1/2")
    assert(part_two(numbers, verbose=True) == out2), "Wrong output for part_two({}) ".format(numbers)
    print("2/2")
    return "OK"

if __name__ == "__main__":
    examples = [
        ([0,3,6], 436, 175594),
        ([1,3,2], 1, 2578),
        ([2,1,3], 10, 3544142),
        ([1,2,3], 27, 261214),
        ([2,3,1], 78, 6895259),
        ([3,2,1], 438, 18),
        ([3,1,2], 1836, 362),
    ]

    # Let's test the examples with multi processing, to get the results faster:
    processes = None
    with Pool(processes) as p:
        print(p.map(test_example, examples))

    # If everything went well, let's get the puzzle result
    puzzle_input = [0,12,6,13,20,1,17]
    print("Part one:", part_one(puzzle_input, verbose=True))
    print("Part two:", part_two(puzzle_input, verbose=True))