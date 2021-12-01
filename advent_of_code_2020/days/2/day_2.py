import re

def valid_one(a, b, symbol, pwd):
    return a <= pwd.count(symbol) <= b

def valid_two(a, b, symbol, pwd):
    return (pwd[a-1] == symbol) != (pwd[b-1] == symbol)

def count_valid_lines(txt, function, verbose=False):
    valids = 0
    for l in txt.split('\n'):
        m = re.match(r"(\d+)-(\d+) (.): (.+)", l)
        a, b = int(m.group(1)), int(m.group(2))
        symbol = m.group(3)
        password = m.group(4)
        if verbose:
            print("a={}, b={}, symbol={}, password={}".format(a, b, symbol, password))
        if function(a, b, symbol, password):
            valids += 1
    return valids

part_one = lambda txt, verbose: count_valid_lines(txt, valid_one, verbose)
part_two = lambda txt, verbose: count_valid_lines(txt, valid_two, verbose)

if __name__ == "__main__":

    with open("input_example_day_2.txt") as f:
        example = f.read()

    assert(part_one(example, verbose=False) == 2)
    assert(part_two(example, verbose=False) == 1)

    # computing the input
    with open("input_day_2.txt") as f:
        input_day_2 = f.read()
    print("Part one:", part_one(input_day_2, False))
    print("Part two:", part_two(input_day_2, False))