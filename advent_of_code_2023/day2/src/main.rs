use regex::Regex;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
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
    // Cube sets are always "red green blue"
    assert_eq!(parse_cube_set("4 red"), [Some(4), None, None]);
    assert_eq!(parse_cube_set("3 red"), [Some(3), None, None]);
    assert_eq!(parse_cube_set("2 green"), [None, Some(2), None]);
    assert_eq!(
        parse_cube_set("1 red, 2 green, 3 blue"),
        [Some(1), Some(2), Some(3)]
    );
    assert_eq!(
        parse_cube_set("3 blue, 1 red, 2 green"),
        [Some(1), Some(2), Some(3)]
    );

    assert_eq!(parse_cube_set("1 blue"), [None, None, Some(1)]);
    assert_eq!(parse_cube_set("1 blue"), [None, None, Some(1)]);

    let s = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    assert_eq!(
        parse_line(s),
        (
            1,
            vec![
                [Some(4), None, Some(3)],
                [Some(1), Some(2), Some(6)],
                [None, Some(2), None]
            ]
        )
    );
    let s = "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    assert_eq!(parse_line(s).0, 12);

    assert_eq!(filter_game_part1([Some(4), None, Some(3)]), true);
    assert_eq!(filter_game_part1([Some(1), Some(2), Some(6)]), true);
    assert_eq!(filter_game_part1([None, Some(2), None]), true);
    assert_eq!(filter_game_part1([Some(13), Some(1), None]), false);
    assert_eq!(filter_game_part1([None, Some(14), None]), false);
    assert_eq!(filter_game_part1([None, None, Some(15)]), false);

    assert_eq!(
        line_value("Game 123: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
        123
    );
    assert_eq!(
        line_value("Game 123: 3 blue, 13 red; 1 red, 2 green, 6 blue; 2 green"),
        0
    );
    assert_eq!(part1(EXAMPLE), 8);
    assert_eq!(part2(EXAMPLE), 2286);

    println!("Tests passed!");
}

fn parse_color(s: &str, color: &str) -> Option<u32> {
    let re_str: &str = &format!("(\\d+) {color}").to_owned();
    let re = Regex::new(re_str).unwrap();
    let m = re.captures_iter(s).next();
    match m {
        Some(caps) => Some(caps[1].parse::<u32>().unwrap()),
        None => None,
    }
}

fn parse_cube_set(s: &str) -> [Option<u32>; 3] {
    ["red", "green", "blue"].map(|c| parse_color(s, c))
}
// Return the id of the line, plus a vector of all [red, green, blue] numbers
fn parse_line(line: &str) -> (u32, Vec<[Option<u32>; 3]>) {
    // fn parse_line(line: &str) -> u32 {
    let mut split = line[5..].split(':');
    let game_id: u32 = split
        .next()
        .expect("no game?")
        .parse::<u32>()
        .expect("no game id?");
    let sets = split
        .next()
        .expect("no cubes?")
        .split("; ")
        .map(parse_cube_set)
        .collect();
    (game_id, sets)
}

fn filter_game_part1(game: [Option<u32>; 3]) -> bool {
    let max = [12, 13, 14];
    fn cmp_option(max: u32, number: Option<u32>) -> bool {
        match number {
            Some(n) => n <= max,
            None => true,
        }
    }
    std::iter::zip(max, game)
        .map(|(m, n)| cmp_option(m, n))
        .fold(true, |acc, b| acc && b)
}

fn line_value(s: &str) -> u32 {
    let (id, games) = parse_line(s);
    let valid = games.iter().map(|g| filter_game_part1(*g)).all(|b| b);
    if valid {
        return id;
    }
    0
}

fn line_value_part2(s: &str) -> u32 {
    let (_, games) = parse_line(s);
    fn max_option(a: Option<u32>, b: Option<u32>) -> Option<u32> {
        match (a, b) {
            (None, _) => b,
            (_, None) => a,
            (Some(i), Some(j)) => Some(std::cmp::max(i, j)),
        }
    }
    // Compute the max for each element of two triplets
    fn max_games(ga: [Option<u32>; 3], gb: [Option<u32>; 3]) -> [Option<u32>; 3] {
        [
            max_option(ga[0], gb[0]),
            max_option(ga[1], gb[1]),
            max_option(ga[2], gb[2]),
        ]
    }

    // For every game I compute the minimum number of balls of each color that we need
    let mg = games
        .iter()
        .fold([None; 3], |accu_max, game| max_games(accu_max, *game));
    // Then the "power" of this game is the multiplication of the number of red, green and blue
    // balls that we need
    mg[0].expect("No red?") * mg[1].expect("No green?") * mg[2].expect("No blue?")
}

fn part1(s: &str) -> u32 {
    s.trim().lines().map(line_value).sum()
}

fn part2(s: &str) -> u32 {
    s.trim().lines().map(line_value_part2).sum()
}
