use regex::Regex;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
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
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    println!("Testing diff...");
    assert_eq!(diff(&vec![1, 1, 1]), vec![0, 0]);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 114);
    println!("Testing part2...");
    assert_eq!(part2(EXAMPLE), 2);
}

fn parse_ints(s: &str) -> Vec<isize> {
    let re = Regex::new("(-?\\d+)").unwrap();
    let mut ints: Vec<isize> = vec![];
    for (_, [s]) in re.captures_iter(s).map(|cap| cap.extract()) {
        ints.push(s.parse().unwrap());
    }
    ints
}

fn parse_input(s: &str) -> Vec<Vec<isize>> {
    s.trim().lines().map(parse_ints).collect()
}

fn diff(l: &Vec<isize>) -> Vec<isize> {
    let right = l[1..].to_vec();
    let left = l[0..(l.len() - 1)].to_vec();
    assert_eq!(left.len(), right.len());
    println!("l={l:?}");
    // println!("right={right:?}");
    // println!("left ={left:?}");
    l[1..]
        .to_vec()
        .iter()
        .zip(l[0..(l.len())].to_vec())
        .collect::<Vec<(&isize, isize)>>()
        .iter()
        .map(|(a, b)| **a - *b)
        .collect()
}

fn prediction(l: &Vec<isize>) -> isize {
    if l.iter().all(|n| *n == 0) {
        0
    } else if l.len() == 1 {
        *l.first().unwrap()
    } else {
        l.last().unwrap() + prediction(&diff(&l))
    }
}

fn part1(s: &str) -> isize {
    parse_input(s)
        .iter()
        .map(|v| {
            let pred = prediction(v);
            println!("pred={pred}");
            pred
        })
        .collect::<Vec<isize>>()
        .iter()
        .sum()
}

fn prediction_part2(l: &Vec<isize>) -> isize {
    let mut tmp = l.clone();
    tmp.reverse();
    prediction(&tmp)
}

fn part2(s: &str) -> isize {
    parse_input(s)
        .iter()
        .map(|v| {
            let pred = prediction_part2(v);
            println!("pred={pred}");
            pred
        })
        .collect::<Vec<isize>>()
        .iter()
        .sum()
}
