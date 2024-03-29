use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

const FIRST_PATTERN: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.";

const SECOND_PATTERN: &str = "
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#";

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    tests();
    println!("Reading the input file...");
    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Computing the result...");
    let res = part1(&contents);
    println!("Part 1: {res}");
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn parse_input(s: &str) -> Vec<&str> {
    s.trim().split("\n\n").collect()
}

fn tests() -> () {
    println!("Starting tests!");
    println!("Testing find_reflections...");
    assert_eq!(find_reflections(".##", None), vec![2]);
    assert_eq!(find_reflections("#.##..##.", None), vec![5, 7]);
    assert_eq!(find_reflections("..#.##.#.", None), vec![1, 5]);
    println!("Testing all_reflections...");
    assert_eq!(all_reflections(FIRST_PATTERN), vec![5]);
    assert_eq!(all_reflections(SECOND_PATTERN), vec![400]);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 405);
    println!("Testing all_smudges_reflections...");
    assert_eq!(all_smudges_reflections(FIRST_PATTERN), 300);
    assert_eq!(all_smudges_reflections(SECOND_PATTERN), 100);
}

fn same_start<T: Eq>(v1: &Vec<T>, v2: &Vec<T>) -> bool {
    v1.iter().zip(v2).all(|t: (&T, &T)| t.0 == t.1)
}

fn find_reflections(line: &str, candidates: Option<Vec<usize>>) -> Vec<usize> {
    // Return the indices `i` for which line[0..i] (reversed) and line[i..] match
    // i.e for "#.##..##." there are axes of reflections:
    // - between the chars at indices 4 and 5 (x.##.|.##.) (we don't care about 'x')
    // - between the cars at indices 6 and 7  (xxxxx.#|#.)
    // so this function will return vec![5,7]
    // Each number in the result represent the number of columns before the axis.
    // println!("line = {line}");
    let mut valid: Vec<usize> = vec![];
    let indices: Vec<usize>;
    if let Some(is) = candidates {
        indices = is
    } else {
        // i=1 means that the chars at indices 0 and 1 are the same
        // i=line.len() - 1 means that the two last chars of line are the same
        indices = (1..line.len()).collect();
    }
    // println!("Indices: {indices:?}");
    for i in indices {
        // i is the number of columns before (on the left or above) the axis
        let rev_start: Vec<char> = line[..i].chars().rev().collect();
        let end: Vec<char> = line[i..].chars().collect();
        // println!("i={i}, rev_start={rev_start:?}, end={end:?}");
        if same_start(&rev_start, &end) {
            valid.push(i);
        }
    }
    // println!("Valid: {valid:?}");
    valid
}

fn all_reflections(s: &str) -> Vec<usize> {
    // Return the scores for reflections of this pattern
    // Example: vec![3, 400] means that there are two axes of reflection:
    // - The first is a vertical one with 3 columns before it
    // - The second is an horizontal one with 4 rows before it
    let split = s.trim().lines().collect::<Vec<&str>>();
    // Searching for a vertical axis of reflection...
    let mut cols_candidates = find_reflections(split.first().unwrap(), None);
    for line in &split[1..] {
        cols_candidates = find_reflections(&line, Some(cols_candidates));
        // println!("cols_candidates: ");
        if cols_candidates.is_empty() {
            break;
        }
    }

    // Searching for an horizontal axis of reflection...
    // Constructing the 1st column
    let mut row_candidates: Option<Vec<usize>> = None;
    let split = s.trim().lines().collect::<Vec<&str>>();
    // println!("split={split:?}");
    for (i, _) in split.first().unwrap().chars().enumerate() {
        let column: String = split
            .iter()
            .map(|v| v.chars().nth(i).unwrap().to_string())
            .collect::<Vec<String>>()
            .join("");
        // println!("Searching column '{column}'...");
        row_candidates = Some(find_reflections(&column, row_candidates));
        // println!("row_candidates={row_candidates:?}");
    }
    cols_candidates.extend(row_candidates.unwrap().iter().map(|n| 100 * n));
    return cols_candidates;
}

fn part1(s: &str) -> usize {
    let mut sum = 0;
    for pattern in parse_input(s) {
        sum += all_reflections(pattern).first().unwrap();
    }
    return sum;
    // parse_input(s)
    //     .iter()
    //     .map(|s| all_reflections(s).clone().first().unwrap())
    //     .sum()
}

fn all_smudges(s: &str) -> Vec<String> {
    let mut new: Vec<String> = vec![];
    for (i, c) in s.chars().enumerate() {
        let new_char: char;
        match c {
            '.' => {
                new_char = '#';
            }
            '#' => {
                new_char = '.';
            }
            _ => continue,
        }
        let mut new_s: String = s[..i].to_string();
        new_s.push(new_char);
        new_s.push_str(&s[i + 1..]);
        new.push(new_s);
    }
    // println!("Built {} variants.", new.len());
    // println!("{new:#?}");
    new
}

fn all_smudges_reflections(s: &str) -> usize {
    let old_reflections = all_reflections(s);
    let old_reflection = old_reflections.first().unwrap();
    println!("Searching variants, old reflection was {old_reflection}");
    for smudge in all_smudges(s) {
        print!("Searching reflections...");
        let v = all_reflections(&smudge);
        println!("Found {} reflections.", v.len());
        if v.is_empty() {
            println!("This smudge does not allow reflections.");
        } else if v.len() == 1 && v.first().unwrap() == old_reflection {
            println!("This smudge has the same reflection as before.");
        } else if v.len() == 1 {
            println!("Ooooh one new reflection!");
            return v.first().unwrap().clone();
        } else if v.len() == 2 {
            println!("Found two axes of reflection: {v:?}");
            if v.first().unwrap() == old_reflection {
                return v[1];
            } else {
                return v.first().unwrap().clone();
            }
        }
    }
    panic!()
}
fn part2(s: &str) -> usize {
    parse_input(s)
        .iter()
        .map(|s| all_smudges_reflections(s))
        .sum()
}
