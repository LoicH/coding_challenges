"""The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
"""

import math

lim = int(2 * 1e6)
# Let's do the Sieve of Eratosthenes
# l[i] = True <=> i is prime
l = [True for n in range(lim)]

# 0 and 1 are not prime
l[0], l[1] = False, False
total_sum = 0
for i in range(lim):
    if not l[i]:
        continue
    total_sum += i
    for j in range(2 * i, lim, i):
        l[j] = False
print(total_sum)
