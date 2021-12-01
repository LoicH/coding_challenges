import re

def part_one(rules, nearby_tickets, verbose=False):
    valid_ranges = [range(rule[0], rule[1]+1) for rule_set in rules.values() for rule in rule_set]
    error_rate = 0
    for t in nearby_tickets:
        for n in t:
            if all([n not in valid_range for valid_range in valid_ranges]):
                error_rate += n
    return error_rate

def valid_field(n, ranges):
    return any(n in range(r[0], r[1]+1) for r in ranges)

def keep_valid_tickets(nearby_tickets, rules):
    # Same as part one, except we remove invalid tickets
    valid_ranges = [range(rule[0], rule[1]+1) for rule_set in rules.values() for rule in rule_set]
    valid_tickets = []
    for t in nearby_tickets:
        valid = True
        for n in t:
            if all([n not in valid_range for valid_range in valid_ranges]):
                valid = False
                break
        if valid:
            valid_tickets.append(t)
    return valid_tickets

def part_two(rules, my_ticket, nearby_tickets, verbose=False):
    possibilities = {field: set(range(len(my_ticket))) for field in rules.keys()}
    definitive_positions = {}
    valid_tickets = keep_valid_tickets(nearby_tickets, rules)
    for pos in range(len(my_ticket)):
        # Remove the fields that are not valid for this position
        for field, ranges in rules.items():
            if field in definitive_positions:
                continue
            for i, ticket in enumerate(valid_tickets):
                if not valid_field(ticket[pos], ranges):
                    if verbose:
                        print("Ticket #{} disables field '{}' for position {} ({} / {})".format(i, field, pos, ticket[pos], ranges))
                    possibilities[field].remove(pos)
                    break
        # Check if there is only one field left for this position
        pos_possibilities = [field for field, positions in possibilities.items() if pos in positions]
        if len(pos_possibilities) == 1:
            def_field = pos_possibilities[0]
            definitive_positions[def_field] = pos
            possibilities.pop(def_field)
            if verbose:
                print("Found the field for position {}: {}".format(pos, def_field))
        # Check if there is only one field in possibilities
        if len(possibilities) == 1:
            if verbose:
                print("Reached the end, last field: {}".format(possibilities))
    
    positions_found = None
    while positions_found != 0:
        sure_positions = {field: positions.pop() for field, positions in possibilities.items() if len(positions) == 1}
        positions_found = len(sure_positions)
        if verbose:
            print("Found {} positions".format(positions_found))
        for field, def_pos in sure_positions.items():
            if field in definitive_positions:
                print("Weird, field {} was already in definitive_positions (value {})".format(field, definitive_positions[field]))
            definitive_positions[field] = def_pos
            possibilities.pop(field)
            for positions in possibilities.values():
                positions.remove(def_pos)

    

    departure_product = 1
    for field, pos in definitive_positions.items():
        if field.startswith("departure"):
            departure_product *= my_ticket[definitive_positions[field]]
    return departure_product

def parse_input(path, verbose=False):
    with open(path) as f:
        input_lines = [line.rstrip() for line in  f.readlines() if len(line.rstrip()) > 0]
    rules = {}
    nearby_tickets = []
    n_break_lines = 0
    rule_pattern = r"(.+): (\d+-\d+) or (\d+-\d+)"
    for line in input_lines:
        # Parse the rules
        if n_break_lines == 0:
            match = re.match(rule_pattern, line)
            if not match:
                n_break_lines += 1
                continue
            field = match.group(1)
            rules[field] = []
            ranges = (match.group(2), match.group(3))
            for r in ranges:
                [a,b] = r.split('-')
                rules[field].append([int(a), int(b)])
        elif n_break_lines == 1:
            if line in ("", "your ticket:"):
                continue
            my_ticket = [int(n) for n in line.split(',')]
            n_break_lines += 1
        else:
            if line in ("", "nearby tickets:"):
                continue
            nearby_tickets.append([int(n) for n in line.split(',')])
    if verbose:
        print("Found {} rules, {} nearby tickets".format(len(rules), len(nearby_tickets)))
        print(rules)
        print("My ticket = {}".format(my_ticket))
        print(nearby_tickets)
    return rules, my_ticket, nearby_tickets

if __name__ == "__main__":
    rules_ex, my_ticket_ex, nearby_tickets_ex = parse_input("input_example.txt", verbose=True)
    assert(part_one(rules_ex, nearby_tickets_ex, verbose=False) == 71), "Wrong output for part one"
    rules_ex2, my_ticket_ex2, nearby_tickets_ex2 = parse_input("input_example_2.txt", verbose=True)
    assert(part_two(rules_ex2, my_ticket_ex2, nearby_tickets_ex2, verbose=False) == 1), "Wrong output for part two"

    rules_puzzle, my_ticket_puzzle, nearby_tickets_puzzle = parse_input("input.txt")
    print("Part one:", part_one(rules_puzzle, nearby_tickets_puzzle, verbose=False))
    print("Part two:", part_two(rules_puzzle, my_ticket_puzzle, nearby_tickets_puzzle, verbose=True))