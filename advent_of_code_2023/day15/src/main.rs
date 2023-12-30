use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

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
    assert_eq!(string_value(&"HASH".to_string()), 52);
    assert_eq!(part1(EXAMPLE), 1320);
    assert_eq!(part2(EXAMPLE), 145);
}

fn string_value(s: &str) -> u16 {
    fn aux(cur: u16, new: u16) -> u16 {
        ((cur + new) * 17) % 256
    }
    s.chars().map(|c| c as u16).fold(0, |a, b| aux(a, b))
}

fn parse_input(s: &str) -> Vec<&str> {
    s.trim().split(",").collect()
}

fn part1(s: &str) -> usize {
    parse_input(s)
        .iter()
        .map(|s| string_value(s) as usize)
        .sum()
}

fn pretty_print(boxes: &Vec<Vec<(&str, &str)>>, pos: &HashMap<&str, (u8, u8)>) -> () {
    println!("Boxes:");
    for (i, v) in boxes.iter().enumerate() {
        if !v.is_empty() {
            println!("{i}: {v:?}");
        }
    }
    println!("Positions:");
    for (label, (b, p)) in pos {
        println!("Label {label} in box #{b} at position {p}");
    }
}
fn part2(s: &str) -> usize {
    let mut boxes: Vec<Vec<(&str, &str)>> = vec![vec![]; 256];
    let mut pos: HashMap<&str, (u8, u8)> = HashMap::new();
    for part in parse_input(s) {
        let l = part.len();
        if part.chars().nth(l - 1).unwrap() == '-' {
            let label = &part[..l - 1];
            // println!("Removing {}...", label);
            if let Some((b, p)) = pos.get(label) {
                let box_nb: usize = b.clone().try_into().unwrap();
                let pos_in_box: usize = (*p).try_into().unwrap();
                boxes[box_nb].remove(pos_in_box);
                for (label, _) in &boxes[box_nb][pos_in_box..] {
                    pos.insert(&label, (pos[label].0, pos[label].1 - 1));
                }
                pos.remove(label);
                // pretty_print(&boxes, &pos);
            } else {
                // println!("This label was not found.");
            }
        } else {
            let op_split: Vec<&str> = part.split('=').collect();
            let label = op_split[0];
            let focal = op_split[1];
            // println!("Adding lense with label={label} and focal={focal}...");
            if let Some((b, p)) = pos.get(label) {
                boxes[*b as usize][*p as usize] = (label, focal);
            } else {
                let h: u8 = string_value(label) as u8;
                pos.insert(label, (h, boxes[h as usize].len() as u8));
                boxes[h as usize].push((label, focal));
            }
            // pretty_print(&boxes, &pos);
        }
    }
    let mut result: usize = 0;
    println!("Computing the power of each lens...");
    for (label, (b, p)) in pos {
        let mut focusing_power: usize = (b as usize + 1) * (p as usize + 1);
        focusing_power *= boxes[b as usize][p as usize].1.parse::<usize>().unwrap();
        println!("{label}: {focusing_power}, result = {result}");
        result += focusing_power;
    }
    result
}
