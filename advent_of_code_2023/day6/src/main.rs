use regex::Regex;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "Time:      7  15   30
Distance:  9  40  200
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
    // let res = part1(&contents);
    // println!("Part 1: {res}");
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    assert_eq!(num_of_winning_ways(&7, &9), 4);
    assert_eq!(num_of_winning_ways(&15, &40), 8);
    assert_eq!(num_of_winning_ways(&30, &200), 9);
    // println!("Testing part1...");
    // assert_eq!(part1(EXAMPLE), 288);
    println!("Testing part2...");
    assert_eq!(part2(EXAMPLE), 71503);
}

// fn part1(s: &str) -> u32 {
//     parse_input(s)
//         .iter()
//         .map(|(t, d)| num_of_winning_ways(t, d))
//         .collect::<Vec<u32>>()
//         .iter()
//         .cloned()
//         .fold(1, |a, x| a * x)
// }
fn part2(s: &str) -> u64 {
    parse_input_part2(s)
        .iter()
        .map(|(t, d)| num_of_winning_ways(t, d))
        .collect::<Vec<u64>>()
        .iter()
        .cloned()
        .fold(1, |a, x| a * x)
}

fn parse_ints(s: &String) -> Vec<u64> {
    let re = Regex::new("(\\d+)").unwrap();
    let mut ints: Vec<u64> = vec![];
    for (_, [s]) in re.captures_iter(s).map(|cap| cap.extract()) {
        ints.push(s.parse().unwrap());
    }
    println!("s={s} & ints={ints:?}");
    ints
}

// fn parse_input(s: &str) -> Vec<(u32, u32)> {
//     let ints: Vec<Vec<u32>> = s
//         .trim()
//         .lines()
//         .map(|l| parse_ints(&String::from(l)))
//         .collect::<Vec<Vec<u64>>>();
//     ints[0]
//         .iter()
//         .zip(&ints[1])
//         .map(|(&a, &b)| (a, b))
//         .collect()
// }

fn remove_spaces(line: &str) -> String {
    line.trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("")
}

fn parse_input_part2(s: &str) -> Vec<(u64, u64)> {
    let ints: Vec<Vec<u64>> = s
        .trim()
        .lines()
        .map(|l| parse_ints(&remove_spaces(&l)))
        .collect::<Vec<Vec<u64>>>();
    ints[0]
        .iter()
        .zip(&ints[1])
        .map(|(&a, &b)| (a, b))
        .collect()
}

fn num_of_winning_ways(time: &u64, dist: &u64) -> u64 {
    let t: f64 = *time as f64;
    let d: f64 = *dist as f64;
    let delta: f64 = t.powf(2.0) - 4.0 * d;
    let sqrt_delta: f64 = delta.sqrt();
    let x1: f64 = (t + sqrt_delta) / 2.0;
    let x2: f64 = (t - sqrt_delta) / 2.0;
    let min_val: u64 = (x2 + 1.0).floor() as u64;
    let max_val: u64 = (x1 - 1.0).ceil() as u64;

    max_val - min_val + 1
}
