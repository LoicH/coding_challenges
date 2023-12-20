use cached::proc_macro::cached;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = ".???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";

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
    let res = part1(&contents, 1);
    println!("Part 1: {res}");
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");

    assert_eq!(line_value("???.### 1,1,3", 1), 1);
    println!("Test passed!");
    assert_eq!(line_value(".??..??...?##. 1,1,3", 1), 4);
    println!("Test passed!");
    assert_eq!(line_value("?#?#?#?#?#?#?#? 1,3,1,6", 1), 1);
    println!("Test passed!");
    assert_eq!(line_value("????.#...#... 4,1,1", 1), 1);
    println!("Test passed!");
    assert_eq!(line_value("????.######..#####. 1,6,5", 1), 4);
    println!("Test passed!");
    assert_eq!(line_value("??? 1", 1), 3);
    println!("Test passed!");
    assert_eq!(line_value("??????? 2,1", 1), 10);
    println!("Test passed!");
    assert_eq!(line_value("?###???????? 3,2,1", 1), 10);
    println!("Test passed!");
    assert_eq!(line_value("???#??????#??. 1,11", 1), 1);
    println!("Test passed!");
    assert_eq!(line_value("???????? 2,1,1", 1), 10);
    println!("Test passed!");
    assert_eq!(line_value("##??#?? 6", 1), 1);
    println!("Test passed!");
    assert_eq!(line_value("??????.??#. 2,3", 1), 5);
    println!("Test passed!");
    assert_eq!(line_value("?.??????##?#??? 1,2,8", 1), 7);
    println!("Test passed!");
    assert_eq!(line_value("##?#???.???? 8,3", 1), 0);
    println!("Test passed!");
    assert_eq!(line_value("?.??????##?#???.???? 1,2,8,3", 1), 14); // to check
    println!("Test passed!");

    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE, 1), 21);
    println!("Test passed!");
    println!("Tests for part2...");

    assert_eq!(line_value_part2("???.### 1,1,3"), 1);
    println!("Test passed!");
    assert_eq!(line_value_part2(".??..??...?##. 1,1,3"), 16384);
    println!("Test passed!");
    assert_eq!(line_value_part2("?#?#?#?#?#?#?#? 1,3,1,6"), 1);
    println!("Test passed!");
    assert_eq!(line_value_part2("????.#...#... 4,1,1"), 16);
    println!("Test passed!");
    assert_eq!(line_value_part2("????.######..#####. 1,6,5"), 2500);
    println!("Test passed!");
    assert_eq!(line_value_part2("?###???????? 3,2,1"), 506250);

    println!("Tests done!");
}

fn parse_line(line: &str) -> (Vec<char>, Vec<usize>) {
    let split: Vec<&str> = line.split(' ').collect();

    (
        split[0].chars().collect(),
        split[1].split(',').map(|c| c.parse().unwrap()).collect(),
    )
}
#[cached]
fn f(s: String, s2: String) -> usize {
    s.len() + s2.len()
}

#[cached]
fn arrangements(s_v: String, s_pos: String) -> usize {
    if s_pos.is_empty() {
        return (!s_v.contains('#')) as usize;
    }
    println!("Line={s_v:?}, pos={s_pos:?}");

    let pos: Vec<usize> = s_pos.split(',').map(|c| c.parse().unwrap()).collect();
    // Quick return if too many positions
    if pos.iter().sum::<usize>() + pos.len() - 1 > s_v.len() {
        println!(
            "The numbers won't fit in {} slots. Stopping there.",
            s_v.len()
        );
        return 0;
    }
    let v: Vec<char> = s_v.chars().collect();
    // Quick return if only '?'
    if pos.len() == 1 {
        let unique: HashSet<char> = HashSet::from_iter(v.clone());
        if unique.len() == 1 && unique.contains(&'?') {
            let n = s_v.len().checked_sub(*pos.first().unwrap());
            if let Some(val) = n {
                println!("{} arrangements", val + 1);
                return val + 1;
            } else {
                return 0;
            }
        }
    }
    match (v.first(), pos.first()) {
        (None, None) => {
            println!("1 arrangement");
            1
        }
        (None, _) | (Some(_), None) => 0,
        (Some('.'), _) => arrangements(s_v[1..].to_string(), s_pos),
        (Some('#'), Some(n)) => {
            // Last number matches perfectly
            if v.len() == *n && !v.contains(&'.') && !s_pos.contains(',') {
                {
                    println!("1 arrangement");
                    1
                }
            } else if v.len() > *n && !v[0..*n].contains(&'.') && v[*n] != '#' {
                let first_comma = s_pos.find(',').unwrap_or(s_pos.len() - 1);
                arrangements(
                    s_v[*n + 1..].to_string(),
                    s_pos[first_comma + 1..].to_string(),
                )
            } else {
                0
            }
        }
        (Some('?'), _) => {
            let mut pound: String = String::from("#");
            let mut dot: String = String::from(".");
            pound.push_str(&s_v[1..]);
            dot.push_str(&s_v[1..]);
            arrangements(pound, s_pos.clone()) + arrangements(dot, s_pos.clone())
        }
        (Some(c), _) => panic!("Encountered character {c}"),
    }
}

fn line_value(line: &str, _n: usize) -> usize {
    let split: Vec<&str> = line.split(' ').collect(); //parse_line(line);
    let (v, pos) = (split[0], split[1]);
    let ret = arrangements(v.to_string(), pos.to_string()); //.repeat(n)
    println!("line='{line}',            arrangements = {ret}");
    ret
}

fn part1(s: &str, _n: usize) -> usize {
    s.trim().lines().map(|l| line_value(l, _n)).sum()
}

fn line_value_part2(line: &str) -> usize {
    let split: Vec<&str> = line.split(' ').collect(); //parse_line(line);
    let (v, pos) = (split[0], split[1]);
    let mut new_v: String = v.to_string();
    let mut new_pos: String = pos.to_string();
    for _ in 1..5 {
        new_v.push('?');
        new_v.push_str(v);
        new_pos.push(',');
        new_pos.push_str(pos);
    }
    let ret = arrangements(new_v, new_pos);
    println!("line='{line}',            arrangements = {ret}");
    ret
}

fn part2(s: &str) -> usize {
    s.trim().lines().map(line_value_part2).sum()
}
