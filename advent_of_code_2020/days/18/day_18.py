import operator

def part_one(lines, verbose=False):
    def evaluate(expr):
        operators = {'+': operator.add,
        '*': operator.mul}
        if verbose:
            print(expr)
        if len(expr) == 1:
            return int(expr[0])
        # expr is reversed, parentheses are too
        if expr[0] == ')':
            # parse until parentheses are balanced
            paren_count = 0
            for i, c in enumerate(expr):
                if c == ')':
                    paren_count += 1
                elif c == '(':
                    paren_count -= 1
                    if paren_count == 0:
                        break
            # expr[i] is ')'
            if i == len(expr) - 1:
                return evaluate(expr[1:-1])
            # else, expr[i+1] is operator, and remainder is expr[i+2:]
            op = expr[i+1]
            return operators[op](evaluate(expr[1:i]), evaluate(expr[i+2:]))
        elif len(expr) > 2 and expr[1] in operators:
            return operators[expr[1]](int(expr[0]), evaluate(expr[2:]))
    if verbose:
        print("Lines to evaluate:")
        print(lines)
    s = 0
    for l in lines:
        if verbose:
            print("Evaluating line {}".format(l))
        reversed_l = l.copy()
        reversed_l.reverse()
        v = evaluate(reversed_l)
        if verbose:
            print("Result: {}".format(v))
        s += v
    return s

def part_two(lines, verbose=False):
    def step(l):
        if '(' in l:
            close_paren = l.index(')')
            open_paren_indices = [i for (i,c) in enumerate(l[:close_paren]) if c == '(']
            open_paren = open_paren_indices[-1]
            # l[open_paren] = '('
            if close_paren == open_paren + 2:
                return l[:open_paren] + [l[open_paren+1]] + l[close_paren+1:]
            elif '+' in l[open_paren+1:close_paren]:
                i = l[open_paren:close_paren].index('+')
                return l[:open_paren+i+-1] + [l[open_paren+i+-1] + l[open_paren+i+1]] + l[open_paren+i+2:]
            else:
                i = l[open_paren:close_paren].index('*')
                return l[:open_paren+i+-1] + [l[open_paren+i+-1] * l[open_paren+i+1]] + l[open_paren+i+2:]
        elif '+' in l:
            i = l.index('+')
            return l[:i-1] + [l[i-1] + l[i+1]] + l[i+2:]
        elif '*' in l:
            i = l.index('*')
            return l[:i-1] + [l[i-1] * l[i+1]] + l[i+2:]
        else:
            return l
    s = 0
    for l in lines:
        if verbose:
            print("Evaluating line {}".format(l))
        current_line = None
        next_line = l
        for (i, elt) in enumerate(l):
            if elt.isnumeric():
                next_line[i] = int(elt)
        while current_line != next_line:
            current_line, next_line = next_line, step(next_line)
            if verbose:
                print("Current: {}, Next: {}".format(current_line, next_line))
        s += next_line[0]
    return s

def parse_input(path, verbose=False):
    with open(path) as f:
        data = [l.rstrip() for l in f.readlines() if len(l.rstrip()) > 0] 

    lines = [parse_line(l) for l in data]
    return lines

def parse_line(l):
    return [c for c in l if c != ' ']

if __name__ == "__main__":
    examples = { "2 * 3 + (4 * 5)": (26, 46),
    "5 + (8 * 3 + 9 + 3 * 4 * 3)": (437, 1445),
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))": (12240, 669060),
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2": (13632, 23340),
    }

    for ex, res in examples.items():
        assert(part_one([parse_line(ex)], verbose=False) == res[0]), "Wrong output for small example {}".format(ex)
        assert(part_two([parse_line(ex)], verbose=False) == res[1]), "Wrong output for small example {}".format(ex)

    input_ex = parse_input("input_example.txt", verbose=True)
    assert(part_one(input_ex, verbose=True) == sum([elt[0] for elt in examples.values()])), "Wrong output for part one"
    assert(part_two(input_ex, verbose=False) == sum([elt[1] for elt in examples.values()])), "Wrong output for part two"

    input_puzzle = parse_input("input_papa.txt")
    print("Part one:", part_one(input_puzzle, verbose=True))
    print("Part two:", part_two(input_puzzle, verbose=False))


