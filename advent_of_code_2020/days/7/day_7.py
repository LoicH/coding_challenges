from collections import defaultdict, deque
import re

def compute_graph(txt, verbose=False):
    # {"bag": (set of bags that hold this one)}
    holders = defaultdict(set)

    contain_pattern = r"(.+) bags contain (.*)\."
    for line in txt.split('\n'):
        if verbose:
            print("line = {}".format(line))
        m = re.match(contain_pattern, line)
        if not m:
            if verbose:
                print("Not 'contain_pattern'")
            continue
        holder = m.group(1)
        content = m.group(2)
        # if verbose:
        #     print("content = {}".format(content))
        for bags in content.split(', '):
            # if verbose:
            #     print("bags={}".format(bags))
            content_pattern = r"\d+ (\S+ \S+) bag"
            match_content = re.match(content_pattern, bags)
            if not match_content:
                if verbose:
                    print("Not 'content_pattern'")
                continue
            bag_inside = match_content.group(1)
            if verbose:
                print("{} is in {}".format(bag_inside, holder))
            holders[bag_inside].add(holder)
    return holders

def compute_solutions(holders, bag="shiny gold", verbose=False):
    solutions = set()
    candidates = deque(holders[bag])

    while len(candidates) > 0:
        candidate = candidates.pop()
        if candidate in solutions:
            if verbose:
                print("Candidate {} already seen".format(candidate))
            continue
        solutions.add(candidate)
        candidates.extend(holders[candidate])
        if verbose:
            print("Added {}, solutions={} and candidates={}".format(candidate, solutions, candidates))
    return len(solutions)


def part_one(txt, verbose=False):
    holders = compute_graph(txt, verbose=verbose)
    if verbose:
        print("holders = {}".format(holders))
    nb_sols = compute_solutions(holders, verbose=verbose)
    return nb_sols

def compute_graph_part_two(txt, verbose=False):
    # {"gold": {"red":3, "green":4}} = gold contains 3 red and 4 green
    graph = defaultdict(dict)

    contain_pattern = r"(.+) bags contain (.*)\."
    for line in txt.split('\n'):
        if verbose:
            print("line = {}".format(line))
        m = re.match(contain_pattern, line)
        if not m:
            if verbose:
                print("Not 'contain_pattern'")
            continue
        holder = m.group(1)
        content = m.group(2)
        # if verbose:
        #     print("content = {}".format(content))
        for bags in content.split(', '):
            # if verbose:
            #     print("bags={}".format(bags))
            content_pattern = r"(\d+) (\S+ \S+) bag"
            match_content = re.match(content_pattern, bags)
            if not match_content:
                if verbose:
                    print("Not 'content_pattern'")
                continue
            nb_bags = match_content.group(1)
            bag_inside = match_content.group(2)
            if verbose:
                print("{} contains {} {} bags ".format(holder, nb_bags, bag_inside))
            graph[holder][bag_inside] = int(nb_bags)
    if verbose:
        print("Graph = {}".format(graph))
    return graph

def count_contained_bags(graph, verbose=False):
    goal = "shiny gold"
    weights = {}
    stack = [goal]
    while len(stack) > 0:
        node = stack.pop()
        if verbose:
            print("Node = {}, stack = {}".format(node, stack))
        if node in weights:
            if verbose:
                print("We know its weight: {}".format(weights[node]))
            continue
        new_sons = graph[node].keys() - weights.keys()
        if len(new_sons) == 0:
            if verbose:
                print("No new son, computing weights: {}".format(weights))
            weight = sum(qty*(1+weights[bag]) for bag, qty in graph[node].items())
            if verbose:
                print("Computed weight = {}".format(weight))
            weights[node] = weight
        else:
            to_add = [node] + list(new_sons)
            if verbose:
                print("Adding {} to the stack".format(to_add))
            stack.extend(to_add)
    return weights[goal]

def part_two(txt, verbose=False):
    graph = compute_graph_part_two(txt, verbose=verbose)
    solution = count_contained_bags(graph, verbose=verbose)
    if verbose:
        print("Must contain {} other bags".format(solution))
    return solution

if __name__ == "__main__":
    with open("input_example_day_7.txt") as f_ex:
        input_example = f_ex.read()

    assert(part_one(input_example, verbose=False) == 4), "Wrong output for part one"

    with open("input_example_day_7_2.txt") as f_ex2:
        input_example2 = f_ex2.read()

    assert(part_two(input_example2, verbose=True) == 126), "Wrong output for part two"

    with open("input_day_7.txt") as f_input:
        input_txt = f_input.read()

    print("Part one: ", part_one(input_txt))
    print("Part one: ", part_two(input_txt))