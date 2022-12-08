from utils import * 

example_input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""
def read_data(s):
    dirs = defaultdict(set)
    sizes = {}
    to_visit = [""]
    seen_dirs = set()
    current_path = ["/"]
    # TODO avoir les bons chemins de dossier 
    for line in s.splitlines():
        unix_path = "/".join(current_path)
        if line.startswith("$"):
            split = line.split(" ")
            if split[1] == "cd":
                if split[2] == "/":
                    current_path = []
                elif split[2] == "..":
                    current_path = current_path[:-1]
                else:
                    d = split[2]
                    current_path.append(d)
                    unix_path = "/".join(current_path)
                    if unix_path not in seen_dirs:
                        to_visit.append(unix_path)
                        seen_dirs.add(unix_path)
        elif line.startswith("dir "):
            d = line.replace("dir ", "")
            dirs[unix_path].add(d)
        else:
            size, fname = line.split(" ")
            sizes[unix_path+"/"+fname] = int(size)
            dirs[unix_path].add(fname)
    return dirs, sizes, to_visit

example_data = read_data(example_input)

# Part 1
def part1(data):
    dirs, sizes, to_visit = data
    sum_small_dirs = 0
    for d in to_visit[::-1]:
        print(f"Computing the size of folder {d}")
        dir_size = sum([sizes[d+"/"+f] for f in dirs[d]])
        if dir_size <= 100000:
            sum_small_dirs += dir_size
        print(f"{d} weighs {dir_size}")
        if d in sizes:
            print(f"{d} weighs now {sizes[d]+dir_size=}")
            sizes[d] += dir_size
        else:
            sizes[d] = dir_size
    return sum_small_dirs


assert part1(example_data) == 95437

puzzle_input = aocd.get_data()

puzzle_data = read_data(puzzle_input)

try:
    aocd.submit(part1(puzzle_data))
except aocd.AocdError:
    print("Already sent an answer?")

# Part 2
def part2(data):
    return None  # TODO


assert part2(example_data) == None  # TODO

aocd.submit(part2(puzzle_data))
