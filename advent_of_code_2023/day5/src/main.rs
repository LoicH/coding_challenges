use regex::Regex;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
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
    println!("Testing parse_mapping...");
    assert_eq!(parse_mapping("50 98 2"), [50, 98, 2]);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 35);
    // println!("Testing part2...");
    // assert_eq!(part2(EXAMPLE), 0);
    println!("Tests passed!");
}

fn parse_ints(s: &str) -> Vec<u32> {
    let re = Regex::new("(\\d+)").unwrap();
    let mut ints: Vec<u32> = vec![];
    for (_, [s]) in re.captures_iter(s).map(|cap| cap.extract()) {
        ints.push(s.parse().unwrap());
    }
    ints
}

fn parse_mapping(s: &str) -> [u32; 3] {
    let nbs = parse_ints(s);
    assert_eq!(nbs.len(), 3);
    nbs.try_into().unwrap()
}

fn parse_input(s: &str) -> (Vec<u32>, Vec<Vec<[u32; 3]>>) {
    let splits: Vec<&str> = s.split("\n\n").collect();
    let seeds = parse_ints(splits[0]);
    let mut categories: Vec<Vec<[u32; 3]>> = Vec::new();
    for cat in &splits[1..] {
        let lines: Vec<&str> = cat.lines().collect();
        categories.push(lines[1..].iter().map(|l| parse_mapping(l)).collect());
    }
    (seeds, categories)
}

fn apply_map(n: u32, mappings: &Vec<[u32; 3]>) -> u32 {
    let mut val: Option<u32> = None;
    println!("n = {n}");
    for [dest, src, range] in mappings {
        if src <= &n && n - src <= *range {
            println!("Matches mapping dest = {dest}, src = {src}, range = {range}");
            val = Some(n - src + dest);
        }
    }
    match val {
        None => n,
        Some(x) => x,
    }
}

fn part1(s: &str) -> u32 {
    let (seeds, all_mappings) = parse_input(s.trim());
    println!("Seeds = {seeds:?}");
    let mut new_vals: Vec<u32> = seeds;
    for mappings in all_mappings {
        new_vals = new_vals.iter().map(|n| apply_map(*n, &mappings)).collect();
    }
    *new_vals.iter().min().unwrap()
}
// fn part2(s: &str) -> u32 {
//     2
// TODO Map intervals! Good luck
// }
