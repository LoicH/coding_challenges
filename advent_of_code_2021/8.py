import aocd
import re
import numpy as np
import itertools
from collections import defaultdict, Counter
from tqdm import tqdm, trange

test_input = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""


def read_data(s):
    return [line.split("|") for line in s.splitlines()]


test_data = read_data(test_input)
assert len(test_data) == 10

# Part 1
def part1(data):
    unique_numbers = (2, 3, 4, 7)
    recognizable_digits = 0
    for _, output_value in data:
        for digit in output_value.split():
            if len(digit) in unique_numbers:
                recognizable_digits += 1
    return recognizable_digits


assert part1(test_data) == 26

raw_data = aocd.get_data()

input_data = read_data(raw_data)
# aocd.submit(part1(input_data))

SEGMENTS = {
    0: "ABCEFG",
    1: "CF",
    2: "ACDEG",
    3: "ACDFG",
    4: "BCDF",
    5: "ABDFG",
    6: "ABDEFG",
    7: "ACF",
    8: "ABCDEFG",
    9: "ABCDFG",
}
REVERSE_SEGMENTS = {v: k for k, v in SEGMENTS.items()}


def infer_segments(input_signals: str) -> dict:
    """Equations for letters
    I'm using the input example of "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb"
    (i) Finding "1" gives two candidates for segments "C" and "F", (here {b, e})
    (ii) The letter for segment "A" is the difference between letters of 7 ({b, d, e})
        and letters of 1 (therefore, segment "A" has the letter "d")
    (iii) "D" is found if you take the letters found in 2 AND 3 AND 5 (numbers with 5 segments),
        and 4: D = {c, d, f} AND {b, c, e, g} = {c}
    (iv) "B" is found in 4 but not in 1, so you can take the letters for 4,
        remove the letters for 1 and remove D, this gives B = {b, c, e, g} - {b, e} - {c} = {g}
    (v) "G" is common in 2, 3 and 5, and we know "A" and "D": G = {c, d, f} - {d} - {c} = {f}
    (vi) "F" is common in 0, 6 and 9 (segments A, B, F, G) and we already know the letters for
        A, B and G: F = {d, e, f, g} - {d, g, f} = {e}
    (vii) The segment C is found in 1, alongside F: C = {b, e} - {e} = {b}
    (viii) E is the only unknown segment: E = {a, b, c, d, e, f, g} - {d, g, b, c, e, f} = {a}
    """
    signals = input_signals.split()
    segments = dict()
    # Find common letters
    length = {n: len(s) for n, s in SEGMENTS.items()}
    nb_to_segment = {
        n: set.intersection(*[set(s) for s in signals if len(s) == n])
        for n in length.values()
    }

    # (ii) Finding "A" = 7 - 1
    segments["A"] = (nb_to_segment[length[7]] - nb_to_segment[length[1]]).pop()
    # (iii) D = common(2, 3, 5) AND 4
    segments["D"] = (nb_to_segment[length[2]] & nb_to_segment[length[4]]).pop()
    # (iv) B = 4 - 1 - D
    segments["B"] = (
        nb_to_segment[length[4]] - nb_to_segment[length[1]] - set(segments["D"])
    ).pop()
    # (v) G = common(2, 3, 5) - A - D
    segments["G"] = (
        nb_to_segment[length[2]] - set(segments["A"]) - set(segments["D"])
    ).pop()
    # (vi) F = common(0, 6, 9) - A - B - G
    segments["F"] = (
        nb_to_segment[length[6]]
        - set(segments["A"])
        - set(segments["B"])
        - set(segments["G"])
    ).pop()
    # (vii) C = 1 - F
    segments["C"] = (nb_to_segment[length[1]] - set(segments["F"])).pop()
    # (viii) the remaining of the 7 segments
    segments["E"] = (nb_to_segment[7] - set(segments.values())).pop()
    return segments


test_segments = infer_segments(
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb"
)
expected = {
    "A": "d",
    "B": "g",
    "C": "b",
    "D": "c",
    "E": "a",
    "F": "e",
    "G": "f",
}
for k, v in expected.items():
    assert test_segments[k] == v, "test_segments[{}] = {} != {}".format(
        k, test_segments[k], v
    )


def resolve_output_values(output_values: str, letters_meaning: dict) -> int:
    total = 0
    for val in output_values.split():
        segments = "".join(sorted([letters_meaning[c] for c in val]))
        n = REVERSE_SEGMENTS[segments]
        total = 10 * total + n
    return int(total)


assert (
    resolve_output_values(
        "fdgacbe cefdb cefbgd gcbe", {v: k for k, v in expected.items()}
    )
    == 8394
)

# Part 2
def part2(data: list) -> int:
    total = 0
    for signals, output_words in data:
        segments = infer_segments(signals)
        total += resolve_output_values(
            output_words, {v: k for k, v in segments.items()}
        )
    return total


assert part2(test_data) == 61229

aocd.submit(part2(input_data))
