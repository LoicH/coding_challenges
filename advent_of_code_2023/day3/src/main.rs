#![allow(dead_code, unused_variables)]
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
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
    // 540324 too low
    // let res = part2(&contents);
    // println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    // Parsing numbers in a line
    assert_eq!(
        parse_line("..123..", 0).0,
        vec![Number {
            value: 123,
            x: 0,
            y: 2
        }]
    );
    assert_eq!(
        parse_line(".123..45...", 0).0,
        vec![
            Number {
                value: 123,
                x: 0,
                y: 1
            },
            Number {
                value: 45,
                x: 0,
                y: 6,
            }
        ]
    );
    // Parsing symbols in a line
    assert_eq!(
        parse_line(".*.", 34).1,
        vec![Symbol {
            repr: '*',
            x: 34,
            y: 1,
        }]
    );
    assert_eq!(
        parse_line(".*+.", 0).1,
        vec![
            Symbol {
                repr: '*',
                x: 0,
                y: 1,
            },
            Symbol {
                repr: '+',
                x: 0,
                y: 2,
            }
        ]
    );
    // Parsing both
    assert_eq!(
        parse_line(".123.$.", 456),
        (
            vec![Number {
                value: 123,
                x: 456,
                y: 1
            }],
            vec![Symbol {
                repr: '$',
                x: 456,
                y: 5
            }]
        )
    );
    // Parsing multiple lines
    assert_eq!(
        parse_input(".123.$.\n*.34..."),
        (
            vec![
                Number {
                    value: 123,
                    x: 0,
                    y: 1
                },
                Number {
                    value: 34,
                    x: 1,
                    y: 2
                }
            ],
            vec![
                Symbol {
                    repr: '$',
                    x: 0,
                    y: 5
                },
                Symbol {
                    repr: '*',
                    x: 1,
                    y: 0
                }
            ]
        )
    );
    // Testing the 1st part on the example
    assert_eq!(part1(EXAMPLE), 4361);
    println!("Tests passed!");
}
#[derive(PartialEq, Debug)]
struct Number {
    value: u32,
    x: usize,
    y: usize,
}

#[derive(PartialEq, Debug)]
struct Symbol {
    repr: char,
    x: usize,
    y: usize,
}

fn parse_line(s: &str, line_nb: usize) -> (Vec<Number>, Vec<Symbol>) {
    let mut n: Option<u32> = None;
    let mut y: usize = 0;
    let mut numbers = Vec::new();
    let mut symbols = Vec::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_digit(10) {
            n = match n {
                Some(val) => Some(val * 10 + c.to_digit(10).unwrap()),
                None => {
                    y = i;
                    Some(c.to_digit(10).unwrap())
                }
            };
            continue;
        }
        if let Some(val) = n {
            numbers.push(Number {
                value: val,
                x: line_nb,
                y: y,
            });
            n = None;
        }
        if c != '.' {
            symbols.push(Symbol {
                repr: c,
                x: line_nb,
                y: i,
            });
        }
    }
    (numbers, symbols)
}

fn parse_input(s: &str) -> (Vec<Number>, Vec<Symbol>) {
    let mut numbers = Vec::new();
    let mut symbols = Vec::new();
    for (i, l) in s.trim().lines().enumerate() {
        let (nb, symb) = parse_line(l, i);
        symbols.extend(symb);
        numbers.extend(nb);
    }
    (numbers, symbols)
}

fn touches(nb: &Number, s: &Symbol) -> bool {
    let nb_digits: usize = (nb.value.checked_ilog10().unwrap() + 1).try_into().unwrap();
    std::ops::Range {
        start: nb.x.saturating_sub(1),
        end: nb.x + 2,
    }
    .contains(&s.x)
        && std::ops::Range {
            start: nb.y.saturating_sub(1),
            end: nb.y + nb_digits + 1,
        }
        .contains(&s.y)
}

fn part1(s: &str) -> u32 {
    let (numbers, symbols) = parse_input(s);
    let mut sum = 0;

    for nb in &numbers {
        println!("Checking if {nb:?} touches an operator...");
        for symb in &symbols {
            // println!("Checking for {symb:?}");
            if touches(&nb, &symb) {
                sum = sum + nb.value;
                println!("Touches {symb:?}!");
                break;
            }
        }
    }
    sum
}
