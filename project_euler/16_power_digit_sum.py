def sum_digits(n):
    return sum(int(n) for n in str(n))


assert sum_digits(2 ** 15) == 26

print(sum_digits(2 ** 1000))
