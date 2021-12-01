import math
import itertools
import tqdm


def increase_sieve(previous, limit):
    previous_l = len(previous)
    new_sieve = previous + [True for i in range(limit - previous_l)]

    for i, prime in enumerate(
        tqdm.tqdm(
            new_sieve[: int(math.sqrt(limit))],
            desc="increase sieve from {} to {}".format(previous_l, limit),
        )
    ):
        if not prime:
            continue
        for j in tqdm.trange(
            max(previous_l, i * i), limit, i, desc="filtering multiples of {}".format(i)
        ):
            new_sieve[j] = False

    return new_sieve


# There is 8 primes below 20
assert sum(1 for b in increase_sieve([False, False], 20) if b) == 8


def list_changing_positions(n_digits):
    pos = []
    for nb_of_indexes in range(3, n_digits, 3):
        pos += list(itertools.combinations(range(1, n_digits), nb_of_indexes))
    return pos


# For 5 digits, there are 4 ways we can change 3 numbers: ???AB, ??A?B, ?A??B, and A???B
assert len(list_changing_positions(5)) == 4


def starting_numbers(n_digits, repl_indexes):
    other_indexes = [i for i in range(n_digits) if i not in repl_indexes]
    numbers_comb = itertools.product(range(10), repeat=len(other_indexes))
    combinations = [
        c
        for c in numbers_comb
        if c[0] % 2 != 0  # if last digit is even then the whole number is even
        and c[0] % 5 != 0  # same for multiples of 5
        # if the starting number if a multiple of 3, then changing 3 numbers will make a multiple of 3
        and sum(c) % 3 != 0
        # If the biggest digit is fixed, it can't be '0'
        and (n_digits - 1 not in other_indexes or c[-1] != 0)
    ]
    numbers = [
        int(sum(i * 10 ** j for i, j in zip(c, other_indexes))) for c in combinations
    ]
    return numbers


# We can fill the first 2 digits of a 4-digit number in 26 ways
assert len(starting_numbers(4, [2, 3])) == 26


def find_first_family(n_primes):
    """Find the first prime that has a family of `n_primes` number"""
    # Init the Eratosthene sieve
    is_prime = [False, False]
    for n_digits in tqdm.trange(6, 7, desc="searching for {} primes".format(n_primes)):
        # Increase the Eratosthene sieve
        is_prime = increase_sieve(is_prime, int(10 ** n_digits))

        candidate_replacement_indexes = list_changing_positions(n_digits)
        # repl_indexes are indexes of repeated numbers we will cycle through
        for repl_indexes in tqdm.tqdm(
            candidate_replacement_indexes,
            desc="replacement indexes for {} digits".format(n_digits),
        ):
            start_nbrs = starting_numbers(n_digits, repl_indexes)
            for start_n in tqdm.tqdm(start_nbrs, desc="starting numbers", leave=False):
                nbr_primes = 10
                first_prime = None
                for i in range(10):
                    n = start_n + int(sum(i * 10 ** j for j in repl_indexes))
                    if not is_prime[n]:
                        nbr_primes -= 1
                        if nbr_primes < n_primes:
                            break
                    elif not first_prime:
                        first_prime = n
                if nbr_primes >= n_primes:
                    return first_prime, n


print(find_first_family(8))
