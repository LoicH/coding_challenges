"""The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
"""


lim = int(2 * 1e6)
# Let's do the Sieve of Eratosthenes
# l[i] = True <=> i is prime
is_prime = [True for n in range(lim)]

# 0 and 1 are not prime
is_prime[0], is_prime[1] = False, False
total_sum = 0
for i in range(lim):
    if not is_prime[i]:
        continue
    total_sum += i
    for j in range(2 * i, lim, i):
        is_prime[j] = False
print(total_sum)
