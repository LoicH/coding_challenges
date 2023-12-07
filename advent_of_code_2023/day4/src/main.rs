#![allow(dead_code, unused_variables)]
use regex::Regex;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
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
    let res = part1(&contents);
    println!("Part 1: {res}");
    // let res = part2(&contents);
    // println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    assert_eq!(
        parse_line("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"),
        (
            HashSet::from([41, 48, 83, 86, 17]),
            HashSet::from([83, 86, 6, 31, 17, 9, 48, 53])
        )
    );
    assert_eq!(game_value(&HashSet::from([1]), &HashSet::from([2])), 0);
    assert_eq!(part1(EXAMPLE), 13);
    println!("Tests passed!");
}

fn parse_line(line: &str) -> (HashSet<u32>, HashSet<u32>) {
    let split: Vec<&str> = line.split([':', '|']).collect();
    let regex = Regex::new("(\\d+)").unwrap();
    let mut winning_numbers: HashSet<u32> = HashSet::new();
    for (_, [s]) in regex.captures_iter(split[1]).map(|caps| caps.extract()) {
        winning_numbers.insert(s.parse().unwrap());
    }
    let mut numbers_i_have: HashSet<u32> = HashSet::new();
    for (_, [s]) in regex.captures_iter(split[2]).map(|caps| caps.extract()) {
        numbers_i_have.insert(s.parse().unwrap());
    }
    (winning_numbers, numbers_i_have)
}

fn parse_input(s: &str) -> Vec<(HashSet<u32>, HashSet<u32>)> {
    s.trim().lines().map(parse_line).collect()
}

fn game_value(a: &HashSet<u32>, b: &HashSet<u32>) -> u32 {
    let nb_in_common: u32 = a.intersection(&b).count().try_into().unwrap();
    match nb_in_common {
        0 => 0,
        n => u32::pow(2, n - 1),
    }
}

fn part1(s: &str) -> u32 {
    parse_input(s).iter().map(|(a, b)| game_value(a, b)).sum()
}
