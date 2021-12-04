import re
import itertools

test_input = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

test_data = test_input.splitlines()

BOARD_HEIGHT = 5
BOARD_WIDTH = 5


def read_data(lines):
    numbers = [int(n) for n in lines[0].split(",")]
    boards = []
    new_board = []
    for i, line in enumerate(lines[2:] + [""]):
        # empty line => finished reading this board
        if line.rstrip().lstrip() == "":
            boards.append(new_board)
            new_board = []
        else:
            new_board.append([int(n) for n in re.split("\s+", line.lstrip().rstrip())])
    return numbers, boards


nb, boards = read_data(test_data)
assert nb[0:3] == [7, 4, 9]
assert nb[-1] == 1
assert len(boards) == 3


def winning_board(board, i, j):
    # check if the line i is all marked or the column j is all marked
    return (
        sum(board[i][col] for col in range(BOARD_WIDTH) if board[i][col]) == 0
        or sum(board[line][j] for line in range(BOARD_HEIGHT) if board[line][j]) == 0
    )


# Part 1
def part1(drawn_numbers, boards):
    for n, board in itertools.product(drawn_numbers, boards):
        for i, j in itertools.product(range(BOARD_HEIGHT), range(BOARD_WIDTH)):
            if board[i][j] == n:
                board[i][j] = 0
                if winning_board(board, i, j):
                    return (
                        sum(
                            board[line][col]
                            for line, col in itertools.product(
                                range(BOARD_HEIGHT), range(BOARD_WIDTH)
                            )
                        )
                        * n
                    )


test_numbers, test_boards = read_data(test_data)
assert part1(test_numbers, test_boards) == 4512

with open("input_4.txt", "r") as fp:
    data = [line for line in fp.readlines()]

input_numbers, input_boards = read_data(data)
print("1st part:", part1(input_numbers, input_boards))

# Part 2
def part2(drawn_numbers, boards):
    new_boards = boards.copy()
    for n in drawn_numbers:
        boards = new_boards.copy()
        for board in boards:
            for i, j in itertools.product(range(BOARD_HEIGHT), range(BOARD_WIDTH)):
                if board[i][j] == n:
                    board[i][j] = None
                    if winning_board(board, i, j) and len(boards) > 1:
                        new_boards.remove(board)
                        break
                    elif winning_board(board, i, j):
                        return (
                            sum(
                                new_boards[0][line][col]
                                for line, col in itertools.product(
                                    range(BOARD_HEIGHT), range(BOARD_WIDTH)
                                )
                                if new_boards[0][line][col]
                            )
                            * n
                        )


test_numbers, test_boards = read_data(test_data)
assert part2(test_numbers, test_boards) == 1924

with open("input_4.txt", "r") as fp:
    data = [line for line in fp.readlines()]

input_numbers, input_boards = read_data(data)
print("2nd part:", part2(input_numbers, input_boards))
