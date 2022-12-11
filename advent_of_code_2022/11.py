from utils import *
import operator
import warnings
import copy

warnings.filterwarnings("error")

example_input = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

ITEMS = "items"
OP = "op"
TEST_DIV = "test_div"
IF_TRUE_DEST = "if_true_dest"
IF_FALSE_DEST = "if_false_dest"
INSPECT = "inspect"


def parse_op(s):
    s = s.replace("  Operation: new = ", "")
    elts = s.split()
    op = {"+": operator.add, "*": operator.mul}[elts[1]]
    if elts[2] == "old":
        f = lambda x: x
    else:
        a = int(elts[2])
        f = lambda x: a
    return lambda x: op(x, f(x))


def parse_monkey(s):
    lines = s.splitlines()
    monkey_id = int(lines[0].replace("Monkey ", "").replace(":", ""))
    starting_items_str = lines[1].replace("  Starting items: ", "").split(", ")
    starting_items = [int(elt) for elt in starting_items_str]
    func_op = parse_op(lines[2])
    test_div = int(lines[3].replace("  Test: divisible by ", ""))
    if_true_dest = int(lines[4].replace("  If true: throw to monkey ", ""))
    if_false_dest = int(lines[5].replace("  If false: throw to monkey ", ""))
    data = {
        monkey_id: {
            ITEMS: np.array(starting_items, dtype=np.int64),
            OP: func_op,
            TEST_DIV: test_div,
            IF_TRUE_DEST: if_true_dest,
            IF_FALSE_DEST: if_false_dest,
            INSPECT: 0,
        }
    }
    return data


def read_data(s):
    data = {}
    for block in s.split("\n\n"):
        data.update(parse_monkey(block))
    return data


example_data = read_data(example_input)
assert len(example_data) == 4

# Part 1


def play_one_round(data, div_three, modulo):
    for monkey_id in range(len(data)):
        monk = data[monkey_id]
        new_items = np.remainder(monk[OP](monk[ITEMS]), modulo)
        if div_three:
            new_items = np.floor_divide(new_items, 3)
        monk[INSPECT] += new_items.size
        go_true_monkey = np.remainder(new_items, monk[TEST_DIV]) == 0
        monk_true = data[monk[IF_TRUE_DEST]]
        monk_true[ITEMS] = np.concatenate([monk_true[ITEMS], new_items[go_true_monkey]])
        go_false_monkey = ~go_true_monkey
        monk_false = data[monk[IF_FALSE_DEST]]
        monk_false[ITEMS] = np.concatenate(
            [monk_false[ITEMS], new_items[go_false_monkey]]
        )
        data[monkey_id][ITEMS] = np.array([])
    return None


def play(rounds, data, div_three):
    modulo = np.prod([v[TEST_DIV] for v in data.values()])
    data = copy.deepcopy(data)
    for i in tqdm(range(rounds), desc=f"Playing {rounds} rounds"):
        play_one_round(data, div_three, modulo)
    inspect = sorted([v[INSPECT] for v in data.values()])
    return inspect[-1] * inspect[-2]


def part1(data):
    return play(20, data, True)


assert part1(example_data) == 10605

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return play(10000, data, False)


p2 = part2(example_data)
assert p2 == 2713310158

aocd.submit(part2(puzzle_data))
