import re

def parse_passport(txt):
    txt = txt.replace('\n', ' ')
    items = txt.split(' ')
    passport = {}
    for item in items:
        [k, v] = item.split(':')
        passport[k] = v
    return passport

def is_valid_passport_part_one(passport, verbose=False):
    return (len(passport) == 8) or (len(passport) == 7 and "cid" not in passport)

def is_valid_passport_part_two(passport, verbose=False):
    year_pattern = r"\d{4}"
    if not (len(passport) == 8) and not (len(passport) == 7 and "cid" not in passport):
        if verbose:
            print("Not enough fields:", list(passport.keys()))
        return False
    elif not re.match(year_pattern, passport["byr"]) or not "1920" <= passport["byr"] <= "2002":
        if verbose:
            print("Birth year:", passport["byr"])
        return False
    elif not re.match(year_pattern, passport["iyr"]) or not "2010" <= passport["iyr"] <= "2020":
        if verbose:
            print("Issue year:", passport["iyr"])
        return False
    elif not re.match(year_pattern, passport["eyr"]) or not "2020" <= passport["eyr"] <= "2030":
        if verbose:
            print("Exp. year:", passport["eyr"])
        return False
    elif not re.match(r"#[\da-f]{6}", passport["hcl"]):
        if verbose:
            print("Hair color:", passport["hcl"])
        return False
    elif passport["ecl"] not in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"):
        if verbose:
            print("eye color:", passport["ecl"])
        return False
    elif not re.match(r"^\d{9}$", passport["pid"]):
        if verbose:
            print("PID:", passport["pid"])
        return False
    else:
        height_match = re.match(r"(\d+)((cm)|(in))", passport["hgt"])
        if not height_match:
            if verbose:
                print("Wrong height format:", passport["hgt"])
            return False
        height = int(height_match.group(1))
        unit = height_match.group(2)
        if not (unit == "cm" and 150 <= int(height) <= 193) and not (unit == "in" and 59 <= int(height) <= 76):
            if verbose:
                print("Wrong height:", passport["hgt"])
            return False
        if verbose:
            print("All good!")
        return True

def check_valid_passports(txt, is_valid_func, verbose=False):
    passports_txt = txt.split("\n\n")
    valid_passports = 0
    for passport_txt in passports_txt:
        passport = parse_passport(passport_txt)
        if verbose:
            print(passport)
        if is_valid_func(passport, verbose=verbose):
            valid_passports += 1
            if verbose:
                print("Valid passport")
        else:
            if verbose:
                print("Invalid passport")

    return valid_passports

part_one = lambda txt, verbose: check_valid_passports(txt, is_valid_passport_part_one, verbose=verbose)
part_two = lambda txt, verbose: check_valid_passports(txt, is_valid_passport_part_two, verbose=verbose)

if __name__ == "__main__":
    with open("input_example_day_4.txt") as f:
        example = f.read()

    assert(part_one(example, verbose=False) == 2)
    assert(part_two(example, verbose=False) == 2)

    # computing the input example
    with open("input_day_4.txt") as f:
        input_day = f.read()
    print("Part one:", part_one(input_day, verbose=False))
    print("Part two:", part_two(input_day, verbose=False))
