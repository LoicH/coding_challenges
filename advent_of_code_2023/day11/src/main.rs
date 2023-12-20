use std::collections::HashSet;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

const GALAXIES: [(usize, usize); 9] = [
    (0, 3),
    (1, 7),
    (2, 0),
    (4, 6),
    (5, 1),
    (6, 9),
    (8, 7),
    (9, 0),
    (9, 4),
];

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
    println!("Testing parse_input...");
    assert_eq!(parse_input(EXAMPLE), GALAXIES);
    println!("Testing insert_rows...");
    assert_eq!(
        insert_rows(GALAXIES.to_vec(), 1),
        vec![
            (0, 3),
            (1, 7),
            (2, 0),
            (5, 6),
            (6, 1),
            (7, 9),
            (10, 7),
            (11, 0),
            (11, 4)
        ]
    );
    println!("Testing insert_cols...");
    assert_eq!(
        insert_columns(GALAXIES.to_vec(), 1),
        vec![
            (2, 0),
            (9, 0),
            (5, 1),
            (0, 4),
            (9, 5),
            (4, 8),
            (1, 9),
            (8, 9),
            (6, 12)
        ]
    );
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 374);
    println!("Tests for part2...");
    assert_eq!(
        insert_rows(GALAXIES.to_vec(), 9),
        vec![
            (0, 3),
            (1, 7),
            (2, 0),
            (13, 6),
            (14, 1),
            (15, 9),
            (26, 7),
            (27, 0),
            (27, 4)
        ]
    );

    assert_eq!(
        HashSet::<(usize, usize)>::from_iter(insert_columns(
            vec![
                (0, 3),
                (1, 7),
                (2, 0),
                (13, 6),
                (14, 1),
                (15, 9),
                (26, 7),
                (27, 0),
                (27, 4)
            ],
            9
        )),
        HashSet::<(usize, usize)>::from_iter(vec![
            (0, 12),
            (1, 25),
            (2, 0),
            (13, 24),
            (14, 1),
            (15, 36),
            (26, 25),
            (27, 0),
            (27, 13)
        ])
    );

    assert_eq!(
        all_distances(&vec![
            (0, 12),
            (1, 25),
            (2, 0),
            (13, 24),
            (14, 1),
            (15, 36),
            (26, 25),
            (27, 0),
            (27, 13)
        ]),
        1030
    );
    assert_eq!(expand(EXAMPLE, 9), 1030);
    assert_eq!(expand(EXAMPLE, 99), 8410);
    println!("Tests done!");
}

fn parse_input(s: &str) -> Vec<(usize, usize)> {
    // Return coordinates of all galaxies
    let mut galaxies: Vec<(usize, usize)> = Vec::new();
    for (i, line) in s.trim().lines().enumerate() {
        galaxies.extend(
            line.match_indices('#')
                .map(|(j, _)| (i, j))
                .collect::<Vec<(usize, usize)>>(),
        );
    }
    galaxies
}

fn insert_rows(galaxies: Vec<(usize, usize)>, n_rows: usize) -> Vec<(usize, usize)> {
    // galaxies need to be sorted by their first coordinate
    let max_x = galaxies.last().unwrap().0;
    let mut new_galaxies = vec![];
    let mut n_rows_added: usize = 0;
    let mut j: usize = 0;
    for i in 0..=max_x {
        println!("i={i}");
        let mut present: bool = false;
        while let Some((gx, gy)) = galaxies.get(j) {
            println!("    j={j}");
            if *gx == i {
                println!("Found a galaxy in this row");
                present = true;
                new_galaxies.push((gx + n_rows_added, *gy));
                j += 1;
            } else if *gx > i {
                println!("Found a galaxy at {gx}");
                if !present {
                    n_rows_added += n_rows;
                    println!("Increased n_rows_added to {n_rows_added}");
                }
                break;
            }
        }
    }
    new_galaxies
}

fn insert_columns(galaxies: Vec<(usize, usize)>, n_rows: usize) -> Vec<(usize, usize)> {
    // We just have to switch coordinates, sort, and insert rows
    let mut galaxies_clone = galaxies
        .clone()
        .iter()
        .map(|(x, y)| (*y, *x))
        .collect::<Vec<(usize, usize)>>();
    galaxies_clone.sort();

    insert_rows(galaxies_clone, n_rows)
        .iter()
        .map(|(x, y)| (*y, *x))
        .collect()
}

fn dist((x1, y1): &(usize, usize), (x2, y2): &(usize, usize)) -> usize {
    let dx: usize = ((*x2 as isize) - (*x1 as isize)).abs() as usize;
    let dy: usize = ((*y2 as isize) - (*y1 as isize)).abs() as usize;
    dx + dy
}

fn all_distances(galaxies: &Vec<(usize, usize)>) -> usize {
    let mut sum: usize = 0;
    let mut clone = galaxies.clone();
    while let Some(g) = clone.pop() {
        for g2 in &clone {
            let d = dist(&g, g2);
            sum += d;
            println!("Distance between {g:?} and {g2:?}: {d}, increasing total to {sum}");
        }
    }

    sum
}

fn expand(s: &str, n_rows: usize) -> usize {
    let galaxies = parse_input(s);
    let moved = insert_columns(insert_rows(galaxies, n_rows), n_rows);
    all_distances(&moved)
}

fn part1(s: &str) -> usize {
    expand(s, 1)
}
fn part2(s: &str) -> usize {
    expand(s, 1000000 - 1)
}
