use num::integer;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";

const EXAMPLE2: &str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
";

const EXAMPLE3: &str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
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
    assert_eq!(part1(EXAMPLE), 2);
    assert_eq!(part1(EXAMPLE2), 6);
    assert_eq!(part2(EXAMPLE3), 6);
}

fn parse_input(s: &str) -> (&str, HashMap<&str, &str>, HashMap<&str, &str>) {
    let split: Vec<&str> = s.lines().collect();
    let mut left: HashMap<&str, &str> = HashMap::new();
    let mut right: HashMap<&str, &str> = HashMap::new();
    for line in &split[2..] {
        let src = line.get(..3).unwrap();
        let dest_left = line.get(7..10).unwrap();
        let dest_right = line.get(12..15).unwrap();
        left.insert(src, dest_left);
        right.insert(src, dest_right);
    }
    (split[0], left, right)
}

fn part1(s: &str) -> u32 {
    let (path, left, right) = parse_input(s);
    let mut node = "AAA";
    let mut i = 0;
    while node != "ZZZ" {
        let dir = path.chars().nth((i % path.len()) as usize).unwrap();
        node = if dir == 'L' {
            left.get(node).unwrap()
        } else {
            right.get(node).unwrap()
        };
        i = i + 1;
    }
    i.try_into().unwrap()
}

fn lcm(a: usize, b: usize) -> usize {
    integer::lcm(a, b)
}

fn part2(s: &str) -> usize {
    let (path, left, right) = parse_input(s);
    let mut starting_nodes: Vec<&&str> = left.keys().filter(|n| n.ends_with('A')).collect();
    let mut cycles: Vec<usize> = vec![];
    for start_node in starting_nodes {
        let mut i = 0;
        let mut node = start_node.clone();
        while !node.ends_with("Z") {
            let dir = path.chars().nth((i % path.len()) as usize).unwrap();
            node = if dir == 'L' {
                left.get(node).unwrap()
            } else {
                right.get(node).unwrap()
            };
            i = i + 1;
        }
        cycles.push(i);
    }

    println!("cycles={cycles:?}");
    let init: usize = 1;
    cycles.iter().fold(init, |a, b| lcm(a, *b))
}
