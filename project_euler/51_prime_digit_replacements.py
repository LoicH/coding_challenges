import math
import itertools

n_digits = 6
lim = int(10 ** n_digits)
# is_prime[i] = True <=> i is prime
is_prime = [True for n in range(lim)]

# 0 and 1 are not prime
is_prime[0], is_prime[1] = False, False
for i in range(lim):
    if not is_prime[i]:
        continue
    for j in range(2 * i, lim, i):
        is_prime[j] = False

last_digits = (1, 3, 7, 9)
digits = range(0, 10)
# Let's check with the exemple of 56**3
nb_of_prime_candidates = 10
for i in digits:
    n = 56003 + i * 110
    if not is_prime[int(n)]:
        nb_of_prime_candidates -= 1
        if nb_of_prime_candidates < 7:
            break
if nb_of_prime_candidates >= 7:
    print(nb_of_prime_candidates, n)


# If the fixed digits don't allow me
# to create a multiple of 3, I'm good.
digits_ok = lambda a, b: (a + b) % 3 != 0
to_fill = {3, 2, 1}
other_digits = set(range(1, n_digits)) - to_fill

# Let's do the Sieve of Eratosthenes

for a, b in itertools.product(digits, last_digits):
    if not digits_ok(a, b):
        continue
    nb_of_prime_candidates = 10
    for i in digits:
        n = int(
            b
            + sum(i * 10 ** j for j in to_fill)
            + sum(a * 10 ** j for j in other_digits)
        )
        if i == 0:
            print(n)
        if not is_prime[n]:
            nb_of_prime_candidates -= 1
            if nb_of_prime_candidates < 8:
                break
    if nb_of_prime_candidates >= 8:
        print(nb_of_prime_candidates, a, b)
