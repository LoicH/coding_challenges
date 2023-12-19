use std::collections::VecDeque;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = ".....
.S-7.
.|.|.
.L-J.
.....
";

const EXAMPLE2: &str = "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
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
    let res = part1(&contents).0;
    println!("Part 1: {res}");
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    println!("Testing find_starting_point...");
    assert_eq!(find_starting_point(EXAMPLE), (1, 1));
    println!("Testing accessible_neighbours...");
    let lines: Vec<String> = EXAMPLE.lines().map(|s| (String::from(s))).collect();
    assert_eq!(accessible_neighbours(1, 3, &lines), vec![(1, 2), (2, 3)]);
    assert_eq!(accessible_neighbours(3, 1, &lines), vec![(3, 2), (2, 1)]);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE).0, 4);
    println!("Testing area_enclosed...");
    assert_eq!(area_enclosed("..|...|..".chars().collect()), 3);
    assert_eq!(area_enclosed("|L-7.".chars().collect()), 0);
    println!("Testing part2...");
    assert_eq!(part2(EXAMPLE2), 4);
    println!("Tests done!");
}

fn find_starting_point(s: &str) -> (usize, usize) {
    for (x, line) in s.lines().enumerate() {
        if let Some(y) = line.find('S') {
            return (x, y);
        }
    }
    panic!("Did not find S");
}

fn accessible_neighbours(x: usize, y: usize, layout: &Vec<String>) -> Vec<(usize, usize)> {
    let letter = layout[x].as_bytes()[y];
    if letter == b'S' {
        return all_neighbours(x, y, letter)
            .iter()
            .filter(|(nx, ny)| accessible_neighbours(*nx, *ny, layout).contains(&(x, y)))
            .copied()
            .collect();
    }
    all_neighbours(x, y, letter)
}

fn all_neighbours(x: usize, y: usize, letter: u8) -> Vec<(usize, usize)> {
    valid_directions(letter)
        .iter()
        // println!("x = {x}, y = {y}, *dx = {}, *dy = {}", *dx, *dy);
        // println!("x+*dx = {}", (x as i32 + *dx) as usize);
        // println!("y+*dy = {}", (y as i32 + *dy) as usize);
        .map(|(dx, dy)| ((x as i32 + *dx) as usize, (y as i32 + *dy) as usize))
        .collect()
}

fn valid_directions(letter: u8) -> Vec<(i32, i32)> {
    match letter {
        b'.' => Vec::<(i32, i32)>::new(),
        b'S' => vec![(0, 1), (0, -1), (-1, 0), (1, 0)],
        b'-' => vec![(0, -1), (0, 1)],
        b'|' => vec![(-1, 0), (1, 0)],
        b'7' => vec![(0, -1), (1, 0)],
        b'J' => vec![(0, -1), (-1, 0)],
        b'L' => vec![(0, 1), (-1, 0)],
        b'F' => vec![(0, 1), (1, 0)],
        _ => panic!(
            "Character '{}' is not recognized",
            String::from_utf8(vec![letter]).unwrap()
        ),
    }
}

fn _pretty_print_layout(distances: &Vec<Vec<Option<usize>>>) {
    for line in distances {
        for d in line {
            match d {
                None => {
                    print!(" ")
                }
                Some(_) => {
                    print!("o")
                }
            }
        }
        println!();
    }
}

fn part1(s: &str) -> (usize, Vec<Vec<Option<usize>>>) {
    // In `distances`:
    // - None => Point not visited
    // - Some(n) => n steps from the start
    let max_x: usize = s.lines().count();
    let max_y: usize = s.lines().nth(0).unwrap().len();
    let mut distances: Vec<Vec<Option<usize>>> = vec![vec![None; max_y]; max_x];
    let lines: Vec<String> = s.lines().map(|s| (String::from(s))).collect();
    let mut to_visit: VecDeque<(usize, usize)> = VecDeque::new();
    let (sx, sy) = find_starting_point(s);
    to_visit.extend(accessible_neighbours(sx, sy, &lines));
    distances[sx][sy] = Some(0);
    let mut max_dist = 0;
    while let Some((cur_x, cur_y)) = to_visit.pop_front() {
        // pretty_print_layout(&distances);
        // println!("Current ({cur_x}, {cur_y})");
        let neighb = accessible_neighbours(cur_x, cur_y, &lines);
        let mut min: Option<usize> = None;
        for (neighb_x, neighb_y) in neighb {
            // println!("  Neighbour ({neighb_x}, {neighb_y}) ");
            match (distances[neighb_x][neighb_y], min) {
                (None, _) => {
                    to_visit.push_back((neighb_x, neighb_y));
                }
                (Some(d), None) => min = Some(1 + d),
                (Some(d), Some(min_val)) => min = Some(std::cmp::min(1 + d, min_val)),
            }
        }
        // println!("Min = {min:#?}");
        distances[cur_x][cur_y] = min;
        if max_dist < min.unwrap() {
            max_dist = min.unwrap();
        }
    }
    // pretty_print_layout(&distances);
    (max_dist, distances)
}

fn area_enclosed(line: Vec<char>) -> usize {
    // Remove all horizontal walls
    let new_line: Vec<&char> = line.iter().filter(|c| c != &&'-').collect();

    let mut s = String::new();
    let _: Vec<_> = new_line.iter().map(|c| s.push(**c)).collect();
    println!("s=    {s}");
    fn aux(line: Vec<&char>, inside_the_loop: bool) -> usize {
        //'S' is always a 'L' or a 'F'
        match (line.get(0), line.get(1)) {
            (Some('.'), _) => inside_the_loop as usize + aux(line[1..].to_vec(), inside_the_loop),
            (Some('L'), Some('7')) | (Some('F'), Some('J')) => {
                aux(line[2..].to_vec(), !inside_the_loop)
            }
            (Some('L'), Some('J')) | (Some('F'), Some('7')) => {
                aux(line[2..].to_vec(), inside_the_loop)
            }
            (Some('|'), _) => aux(line[1..].to_vec(), !inside_the_loop),
            (None, _) => 0,
            (a, b) => panic!("Encountered characters {a:?} then {b:?}"),
        }
    }
    aux(new_line, false)
}

fn s_could_be(letter: u8, x: usize, y: usize, neighb: &Vec<(usize, usize)>) -> bool {
    valid_directions(letter)
        .iter()
        .map(|(dx, dy)| (((x as i32 + *dx) as usize), ((y as i32 + *dy) as usize)))
        .all(|t| neighb.contains(&t))
}

fn replace_starting_point(s: &str) -> char {
    let (x, y) = find_starting_point(s);
    let layout: Vec<String> = s.lines().map(|s| (String::from(s))).collect();
    let neighb = accessible_neighbours(x, y, &layout);
    "-|FLJ7"
        .bytes()
        .filter(|c| s_could_be(*c, x, y, &neighb))
        .nth(0)
        .unwrap()
        .try_into()
        .unwrap()
}

fn part2(s: &str) -> usize {
    let (_, distances) = part1(s);
    let mut count = 0;
    let new_s = s.replace('S', &replace_starting_point(s).to_string());
    for (i, (line, dist_line)) in new_s.lines().zip(distances).enumerate() {
        println!("Line {i}: {line}");
        let filtered_line: Vec<char> = line
            .chars()
            .zip(dist_line)
            .map(|(c, d)| if d == None { '.' } else { c })
            .collect();
        count += area_enclosed(filtered_line);
        println!("count={count}");
    }

    count
}
