

def count_questions_part_one(group, verbose=False):
    return len(set(group.replace('\n', '')))

def count_questions_part_two(group, verbose=False):
    lines = group.split('\n')
    questions = set(lines[0])
    if verbose:
        print("First line={}, questions={}".format(lines[0], questions))
    for i, line in enumerate(lines[1:]):
        questions = questions & set(line)
        if verbose:
            print("Line {}, questions={}".format(i+1, questions))
    return len(questions)

def sum_groups(txt, func_count_questions, verbose=False):
    total_sum = 0
    for i, group in enumerate(txt.split("\n\n")):
        if verbose:
            print("Count questions for group #{}".format(i))
        n_questions = func_count_questions(group, verbose=verbose)
        total_sum += n_questions
        if verbose: 
            print("Group #{} answered {} questions, total is now {}".format(i, n_questions, total_sum))
    return total_sum

part_one = lambda t, verbose: sum_groups(t, count_questions_part_one, verbose=verbose)
part_two = lambda t, verbose: sum_groups(t, count_questions_part_two, verbose=verbose)

if __name__ == "__main__":
        
    with open("input_example_day_6.txt") as f_ex:
        txt_example = f_ex.read()

    assert(part_one(txt_example, verbose=False) == 11), "Wrong output for part one"
    assert(part_two(txt_example, verbose=False) == 6), "Wrong output for part two"

    with open("input_day_6.txt") as f_in:
        txt_input = f_in.read()
    
    print("Part one:", part_one(txt_input, verbose=False))
    print("Part two:", part_two(txt_input, verbose=False))