use std::fs::File;
use std::io::Read;
use std::str;

const EXAMPLE2: &str = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";
const DIGITS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn main() -> std::io::Result<()> {
    // tests
    assert_eq!(extract_digit("two"), Some(2));
    assert_eq!(extract_digit("two1"), Some(2));
    assert_eq!(extract_digit("two1nine"), Some(2));
    assert_eq!(extract_digit("eight"), Some(8));
    assert_eq!(first_and_last_digit_part2("twonine"), 29);
    assert_eq!(first_and_last_digit_part2("two1nine"), 29);
    assert_eq!(
        first_and_last_digit("lpbjvpbtdfvtxtdvkpjs7qrvddkzmjtlqtg"),
        77
    );

    println!("Testing on example...");
    let n = part2(EXAMPLE2);
    println!("Result: {n}");
    assert_eq!(n, 281);

    // part 2
    println!("Reading input file...");
    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let res = part2(&contents);
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

fn extract_digit(s: &str) -> Option<u32> {
    println!("In extract_digit, s={s}");
    let c = s.chars().next().unwrap();
    match c.to_digit(10) {
        Some(n) => Some(n),
        None => {
            let pos: Option<usize> = DIGITS.map(|d| s.starts_with(d)).iter().position(|&b| b);
            println!("pos={pos:?}");
            match pos {
                Some(p) => Some((p + 1).try_into().unwrap()),
                None => None,
            }
        }
    }
}

fn first_and_last_digit_part2(line: &str) -> u32 {
    let mut i: usize = 0;
    let mut a: Option<u32> = None;
    let mut b: Option<u32> = None;

    while i < line.len() {
        let sub: &str = line.get(i..).expect("no line??");
        match extract_digit(sub) {
            Some(val) => {
                println!("val={val:?}");
                b = Some(val.try_into().unwrap());
                a = match a {
                    Some(_) => a,
                    _ => Some(val.try_into().unwrap()),
                };
            }
            None => {
                println!("Nothing found, going on.");
            }
        }
        i = i + 1;
        println!("i={i}");
    }

    a.unwrap() * 10 + b.unwrap()
}

fn part2(s: &str) -> u32 {
    let split = s.trim().split('\n');
    let nums = split.map(first_and_last_digit_part2);
    nums.sum()
}
