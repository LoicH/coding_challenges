use std::fs::File;
use std::io::Read;
use std::str;

const EXAMPLE: &str = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";

fn main() -> std::io::Result<()> {
    let n = part1(EXAMPLE);
    println!("Result: {n}");

    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let res = part1(&contents);
    println!("Result: {res}");
    Ok(())
}

fn first_and_last_digit(line: &str) -> u32 {
    let mut a: Option<u32> = None;
    let mut b: Option<u32> = None;
    for c in line.chars() {
        if c.is_digit(10) {
            a = match a {
                Some(_) => a,
                _ => c.to_digit(10),
            };
            b = match a {
                Some(_) => c.to_digit(10),
                _ => b,
            };
        }
    }
    println!("a = {a:?}");
    a.unwrap() * 10 + b.unwrap()
}

fn part1(s: &str) -> u32 {
    let split = s.trim().split('\n');
    let nums = split.map(first_and_last_digit);
    nums.sum()
}
