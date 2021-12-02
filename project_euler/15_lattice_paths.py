"""
For a N*N grid, we must travel N times to the East and N times to the South,
se we need to count how many ways there are to insert N East steps into a
path of 2*N steps: it's the binomial coefficient, "2N C N"
"""

from math import factorial as fac


def binomial(k, n):
    return fac(n) / (fac(k) * fac(n - k))


def lattice(n):
    return int(binomial(n, 2 * n))


assert lattice(2) == 6
print(lattice(20))
