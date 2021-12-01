from tqdm import tqdm


def next_step(n):
    return n / 2 if n % 2 == 0 else 3 * n + 1


def collatz_length(n):
    length = 1
    while n != 1:
        n = next_step(n)
        length += 1
    return length


assert collatz_length(13) == 10

longest = 0
longest_i = None
for i in tqdm(range(2, int(1e6))):
    c = collatz_length(i)
    if c > longest:
        longest = c
        longest_i = i

print(longest_i, longest)
