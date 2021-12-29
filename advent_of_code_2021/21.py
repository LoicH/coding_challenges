from utils import *

example_input = """Player 1 starting position: 4
Player 2 starting position: 8"""


def read_data(s):
    return [int(line[-1]) for line in s.split("\n")]


example_data = read_data(example_input)


def deterministic_dice():
    while True:
        yield from iter(range(1, 101))


# Part 1
def part1(data):
    scores = [0, 0]
    positions = data.copy()
    player = 0
    dice = deterministic_dice()
    nb_rolls = 0
    while max(scores) < 1000:
        roll = next(dice) + next(dice) + next(dice)
        nb_rolls += 3
        # Position goes from 1 to 10 then back to 1
        positions[player] = (positions[player] + roll - 1) % 10 + 1
        scores[player] += positions[player]
        player = 1 - player
    return min(scores) * nb_rolls


assert part1(example_data) == 739785

puzzle_input = aocd.get_data(day=21)

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data), day=21)
except aocd.AocdError:
    print("Already sent an answer?")


##############
### Part 2 ###
##############


@lru_cache
def play(score_1, position_1, score_2, position_2):
    """Play the game until we find a winner
    Parameters
    - score_1: score of the player that just played
    - position_1: position of the player that just played
    - score_2: score of the player that will play this round
    - position_2: position of the player that will play this round

    Returns (number of victories of player 1, number of victories of player 2)"""
    if score_1 >= 21:
        # If the last player won in this universe, we return 1 victory for this player
        return (1, 0)
    # The player that plays will roll three 3-faced dices.
    # This player can advance by 3 spaces in 1 universe (they need to roll 1, then 1 and 1 again),
    # or by 4 spaces in 3 universes (they can roll 1,1,2, in different orders)
    possibilites = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
    victories_1 = 0
    victories_2 = 0
    for roll, n_universes in possibilites:
        # Positions go from 1 to 10 and back to 1 again
        new_pos_2 = (position_2 + roll - 1) % 10 + 1
        new_score_2 = score_2 + new_pos_2
        # We compute the number of victories in the universe(s) where player 2 advances of `roll` cases
        new_vic2, new_vic1 = play(new_score_2, new_pos_2, score_1, position_1)
        victories_1 += n_universes * new_vic1
        victories_2 += n_universes * new_vic2
    return (victories_1, victories_2)


# Some scenarios:
assert play(26, 6, 12, 7) == (1, 0)  # Player 1 has played last, and wins
assert play(12, 7, 20, 3) == (0, 27)  # Player 2 will win in any of the 27 universes
# In the next example, player 1 will win in its next play
# So player 2 opens 27 new universes, in each of these universes, player 1 will win any
# of the 27 possibilities. So player 1 wins in 27*27=729 universes
assert play(20, 3, 5, 4) == (729, 0)
# Next example:
# If player 2 rolls advances of 4 spaces or more, they will win: so 26 universes where they win.
# If they advance of 4 spaces, we're in the example above where they win in 729 universes
# So player 2 will win in 755 universes.
assert play(5, 4, 17, 10) == (0, 755)

# I trust my code for these next two examples
# assert play(17, 10, 1, 1) == (20385, 0)
# assert play(7, 7, 0, 8) == (1999636718756, 544483139239)
print("Small tests passed for part 2")

print("Let's compute run times...")
print("Computing play(17, 10, 1, 1)...")
print(timeit.repeat(lambda: play(17, 10, 1, 1), number=10, repeat=3))
print("Computing play(1, 1, 7, 7)...")
print(timeit.repeat(lambda: play(1, 1, 7, 7), number=10, repeat=3))
print("Run times computed.")

# Part 2
def part2(data):
    # Let's see what are the final scores if we start in the starting positions
    # With scores of 0, and player 1 will be the first to play:
    return play(0, data[1], 0, data[0])


part2_example = part2(example_data)
assert max(part2_example) == 444356092776315
print("Example tests passed for part 2")

part2_puzzle = part2(puzzle_data)
aocd.submit(max(part2_puzzle), day=21)
