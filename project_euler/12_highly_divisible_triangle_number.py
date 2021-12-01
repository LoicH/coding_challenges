import math


def nb_of_divisors(n):
    nb = 1
    for i in range(2, int(math.sqrt(n)) + 1):
        if n % i == 0:
            nb += 1
    return 2 * nb


divs = {6: 4, 28: 6}
for k, v in divs.items():
    assert nb_of_divisors(k) == v


triangle_number = 1
i = 2
while nb_of_divisors(triangle_number) < 500:
    triangle_number += i
    i += 1

print(triangle_number)
