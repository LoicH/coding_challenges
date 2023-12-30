use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

const EXAMPLE: &str = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

const STEPS_PART2: usize = 1000000000;

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
    let res = part2(&contents, STEPS_PART2 + 1);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    println!("Testing move_blocks...");
    assert_eq!(*move_blocks(&".O#".to_string()), "O.#".to_string());
    assert_eq!(*move_blocks(&"#.O".to_string()), "#O.".to_string());
    assert_eq!(*move_blocks(&".#O".to_string()), ".#O".to_string());
    assert_eq!(*move_blocks(&".O#O".to_string()), "O.#O".to_string());
    assert_eq!(*move_blocks(&"O.O#O".to_string()), "OO.#O".to_string());
    assert_eq!(*move_blocks(&".#..O".to_string()), ".#O..".to_string());
    println!("Testing compute_load...");
    assert_eq!(compute_load(&"O.".to_string()), 2);
    assert_eq!(compute_load(&"O..".to_string()), 3);
    assert_eq!(compute_load(&"O.#O".to_string()), 5);
    assert_eq!(compute_load(&"O.#O..".to_string()), 9);
    assert_eq!(compute_load(&"O.#.#.#".to_string()), 7);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 136);
    println!("Testing part2...");
    assert_eq!(part2("...\nO#.\n..O", 10), 3);
    assert_eq!(part2("...\n...\nO#.\n..O", 10), 4);
    assert_eq!(part2("...\n...\nO#.\n..O", 1000000000), 4);
    assert_eq!(part2("...\n...\nO#.\n..O", 1000000001), 3);
    assert_eq!(part2(EXAMPLE, 4), 69);
    assert_eq!(part2(EXAMPLE, 11), 69);
    assert_eq!(part2(EXAMPLE, 13), 65);
    assert_eq!(part2(EXAMPLE, STEPS_PART2 + 1), 64);
}

fn parse_input(s: &str) -> Vec<&str> {
    s.trim().split("\n").collect()
}

fn reverse_each(lines: &Vec<String>) -> Vec<String> {
    // Reversing lines (or columns) allow us to move blocks in another direction
    lines
        .iter()
        .map(|l| l.chars().rev().collect::<String>())
        .collect()
}

fn transpose(lines: &Vec<String>) -> Vec<String> {
    // Transposing a vector of lines allows to have the representation of columns
    // Which means that if we input a map of the platform, the transposition is a list of columns, each ready
    // to be sorted, in order to push the boulders to the North.
    let mut cols: Vec<String> = vec![];
    // println!("split={split:?}");
    for (i, _) in lines.first().unwrap().chars().enumerate() {
        cols.push(
            lines
                .iter()
                .map(|v| v.chars().nth(i).unwrap().to_string())
                .collect::<Vec<String>>()
                .join(""),
        );
    }
    cols
}

fn compute_load(line: &String) -> usize {
    let n = line.len();
    line.char_indices()
        .map(|(i, c)| if c == 'O' { n - i } else { 0 })
        .sum()
}

fn sort_reverse(s: &str) -> String {
    let mut chars: Vec<char> = s.chars().collect();
    chars.sort();
    chars.reverse();
    chars
        .iter()
        .map(|c| c.to_string())
        .collect::<Vec<String>>()
        .join("")
}

fn move_blocks(line: &String) -> String {
    return line
        .split("#")
        .collect::<Vec<&str>>()
        .iter()
        .map(|p| sort_reverse(p))
        .collect::<Vec<String>>()
        .join("#");
}

fn part1(s: &str) -> usize {
    let map: Vec<String> = parse_input(s).iter().map(|s| (s.to_string())).collect();
    // We transform the map (a vector of lines) into its transposition to have a vector of columns
    transpose(&map)
        .iter()
        // Then for each column we can move_blocks, i.e. push the blocks as far to the North as possible.
        .map(|l| compute_load(&move_blocks(l)))
        // And compute the load for each column
        .sum()
}

fn cycle(original_west_map: &Vec<String>) -> Vec<String> {
    // Each cycle tilts the platform four times so that the rounded rocks roll north, then west, then south, then east.
    // West to North: transpose
    let mut north_map = transpose(&original_west_map);
    north_map = north_map.iter().map(move_blocks).collect();
    // North to West: transpose back
    let mut west_map = transpose(&north_map);
    west_map = west_map.iter().map(move_blocks).collect();
    // West to South: transpose then reverse
    let mut south_map = transpose(&west_map);
    south_map = reverse_each(&south_map);
    south_map = south_map.iter().map(move_blocks).collect();
    // South to East: reverse, transpose, reverse (i.e. go back to West then reverse)
    let mut east_map = reverse_each(&south_map);
    east_map = reverse_each(&transpose(&east_map));
    east_map = east_map.iter().map(move_blocks).collect();
    // East to West: reverse
    return reverse_each(&east_map);
}

fn part2(s: &str, max_steps: usize) -> usize {
    let mut west_map: Vec<String> = parse_input(s).iter().map(|s| (s.to_string())).collect();
    let mut new_west_map: Vec<String>;
    // Let's memorize the positions of the maps we already encountered
    let mut positions: HashMap<Vec<String>, usize> = HashMap::new();
    // And let's store the history of the maps
    let mut history: Vec<Vec<String>> = Vec::new(); //vec![&west_map];
    println!("Original map: {west_map:#?}");
    for i in 0..max_steps {
        println!("Map of step {i}");
        println!("{west_map:#?}");
        // Checking if we are in a cycle
        if let Some(cycle_start) = positions.get(&west_map) {
            println!("Hey! We already encountered this map!");
            let cycle_len = i - cycle_start;
            let steps_left = max_steps - 1 - i;
            let cycle_steps_left = steps_left % cycle_len;
            println!("Found a cycle starting at step {cycle_start}, of length {cycle_len}.");
            println!("There are {steps_left} steps left, which mean {cycle_steps_left} steps in the cycle.");
            println!(
                "Returning the map of the step {}",
                cycle_start + cycle_steps_left
            );
            west_map = history[cycle_start + cycle_steps_left].to_vec();
            break;
        }
        // Storing the map:
        positions.insert(west_map.clone(), i);
        history.push(west_map.clone());
        println!(
            "Load={}",
            transpose(&west_map).iter().map(compute_load).sum::<usize>()
        );
        // Computing the next step
        new_west_map = cycle(&west_map);
        west_map = new_west_map;
    }
    transpose(&west_map)
        .iter()
        // Then for each column we can move_blocks, i.e. push the blocks as far to the North as possible.
        .map(compute_load)
        // And compute the load for each column
        .sum::<usize>()
}
