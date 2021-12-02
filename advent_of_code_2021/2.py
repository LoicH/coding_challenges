test_input = """forward 5
down 5
forward 8
up 3
down 8
forward 2"""

test_data = [line for line in test_input.splitlines()]


# Part 1
def part1(data):
    horiz_pos = 0
    depth = 0
    for line in data:
        command, n = line.split()
        n = int(n)
        if command == "forward":
            horiz_pos += n
        elif command == "down":
            depth += n
        elif command == "up":
            depth -= n
        else:
            print("Command not recognized on line '{}'".format(line))
    return horiz_pos * depth


assert part1(test_data) == 150

with open("input_2.txt", "r") as fp:
    data = [line for line in fp.readlines()]

print("1st part:", part1(data))


# Part 2
def part2(data):
    horiz_pos = 0
    aim = 0
    depth = 0
    for line in data:
        command, n = line.split()
        n = int(n)
        if command == "forward":
            horiz_pos += n
            depth += n * aim
        elif command == "down":
            aim += n
        elif command == "up":
            aim -= n
        else:
            print("Command not recognized on line '{}'".format(line))
    return horiz_pos * depth


assert part2(test_data) == 900
print("2nd part:", part2(data))
