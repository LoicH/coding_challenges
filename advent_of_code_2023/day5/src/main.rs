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
    let res = part2(&contents);
    println!("Part 2: {res}");
    Ok(())
}

fn tests() -> () {
    println!("Starting tests!");
    println!("Testing parse_mapping...");
    assert_eq!(parse_mapping("50 98 2"), [50, 98, 2]);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 35);

    println!("Testing interval_intersection...");
    // When interval is on the left of the mapping
    assert_eq!(interval_intersection(&(3, 4), &[100, 10, 3]), vec![(3, 4)]);
    // When the interval is on the right on the mapping
    assert_eq!(
        interval_intersection(&(30, 4), &[100, 10, 3]),
        vec![(30, 4)]
    );
    // When the biggest numbers of the interval are mapped
    assert_eq!(
        interval_intersection(&(3, 4), &[100, 5, 3]),
        vec![(3, 2), (100, 2)]
    );
    // When the lowest numbers of the interval are mapped
    assert_eq!(
        interval_intersection(&(3, 4), &[100, 2, 3]),
        vec![(5, 2), (101, 2)]
    );
    // The lowest and highest numbers are not mapped but the middle ones are
    assert_eq!(
        interval_intersection(&(3, 6), &[100, 5, 2]),
        vec![(3, 2), (7, 2), (100, 2)]
    );

    println!("Testing part2...");
    assert_eq!(part2(EXAMPLE), 46);
    println!("Tests passed!");
}

fn parse_ints(s: &str) -> Vec<usize> {
    let re = Regex::new("(\\d+)").unwrap();
    let mut ints: Vec<usize> = vec![];
    for (_, [s]) in re.captures_iter(s).map(|cap| cap.extract()) {
        ints.push(s.parse().unwrap());
    }
    ints
}

fn parse_mapping(s: &str) -> [usize; 3] {
    let nbs = parse_ints(s);
    assert_eq!(nbs.len(), 3);
    nbs.try_into().unwrap()
}

fn parse_input(s: &str) -> (Vec<usize>, Vec<Vec<[usize; 3]>>) {
    let splits: Vec<&str> = s.split("\n\n").collect();
    let seeds = parse_ints(splits[0]);
    let mut categories: Vec<Vec<[usize; 3]>> = Vec::new();
    for cat in &splits[1..] {
        let lines: Vec<&str> = cat.lines().collect();
        categories.push(lines[1..].iter().map(|l| parse_mapping(l)).collect());
    }
    (seeds, categories)
}

fn apply_map(n: usize, mappings: &Vec<[usize; 3]>) -> usize {
    let mut val: Option<usize> = None;
    for [dest, src, range] in mappings {
        if src <= &n && n - src <= *range {
            val = Some(n - src + dest);
        }
    }
    match val {
        None => n,
        Some(x) => x,
    }
}

fn part1(s: &str) -> usize {
    let (seeds, all_mappings) = parse_input(s.trim());
    let mut new_vals: Vec<usize> = seeds;
    for mappings in all_mappings {
        new_vals = new_vals.iter().map(|n| apply_map(*n, &mappings)).collect();
    }
    *new_vals.iter().min().unwrap()
}

fn interval_intersection(interval: &(usize, usize), mapping: &[usize; 3]) -> Vec<(usize, usize)> {
    let (_start_a, _range_a) = *interval;
    let [_dest_b, _start_b, _range_b] = *mapping;
    // No change in the starting interval: return as is
    if _start_a + _range_a <= _start_b || _start_b + _range_b <= _start_a {
        vec![(_start_a, _range_a)]
    }
    // The starting interval is inside the mapping: just shift the interval
    else if _start_b <= _start_a && _start_a + _range_a <= _start_b + _range_b {
        vec![(_start_a - _start_b + _dest_b, _range_a)]
    }
    // The starting interval shares its biggest numbers with the mapping:
    // Return two intervals, one untouched, the other shifted
    // Example: interval=(10, 5) = 10 11 12 13 14 and mapping=(5, 12, 3)= {12 13 14=>5 6 7}
    // Return vec![(10, 2), (5, 3)]
    else if _start_b < _start_a + _range_a && _start_a + _range_a <= _start_b + _range_b {
        let max_common = _start_a + _range_a - 1; // 14 in our example
        let nb_in_common = max_common - _start_b + 1; // 3 in our example

        vec![(_start_a, _range_a - nb_in_common), (_dest_b, nb_in_common)]
    }
    // The starting interval shares its lowest numbers with the mapping:
    // Ex: interval=(10, 5) and mapping=(5, 7, 6) = {7 8 9 10 11 12 => 5 6 7 8 9 10}
    // Return vec![(13, 2), (8, 3)]
    else if _start_b <= _start_a && _start_a < _start_b + _range_b {
        vec![
            (
                _start_b + _range_b,
                _start_a + _range_a - _start_b - _range_b,
            ),
            (
                _start_a - _start_b + _dest_b,
                _start_b + _range_b - _start_a,
            ),
        ]
    }
    // The mapping is inside the starting interval. We will split the interval in 3
    // Ex: interval=(10 5) and mapping=(1, 11, 2) = {11 12 => 1 2}
    // Return vec![(1,2), (10,1), (13,2)]
    else {
        vec![
            (_start_a, _start_b - _start_a),
            (_dest_b, _range_b),
            (
                _start_b + _range_b,
                _start_a + _range_a - _start_b + _range_b,
            ),
        ]
    }
}

fn min_range(intervals: &Vec<(usize, usize)>) -> usize {
    let mut my_min: Option<usize> = None;
    for (a, _) in intervals {
        my_min = match my_min {
            None => Some(*a),
            Some(v) => Some(std::cmp::min(*a, v)),
        };
    }
    my_min.unwrap()
}

fn seeds_to_interval(seeds: Vec<usize>) -> Vec<(usize, usize)> {
    let mut intervals: Vec<(usize, usize)> = Vec::new();
    // Computing intervals (start, range) from the first line of the input
    let mut i = 0;
    (while i < seeds.len() {
        intervals.push((seeds[i], seeds[i + 1]));
        i = i + 2;
    });
    intervals
}

// We have our starting intervals which consist of (starting_number, number_of_elements)
// We have all of the mappings for all categories
// We can iterate over the categories
// For each mappings in all_mappings
// We can iterate over the cross product of the intervals vector and mappings vector
// Then sort the resulting intervals
// Then reassemble the intervals that are next to each other
fn part2(s: &str) -> usize {
    let (seeds, all_mappings) = parse_input(s.trim());

    let intervals = seeds_to_interval(seeds);
    (for mappings in all_mappings {
        let mut new_intervals: Vec<(usize, usize)> = Vec::new();
        for interval in &intervals {
            for mapping in &mappings {
                new_intervals.extend(interval_intersection(interval, mapping));
            }
        }
        let _intervals = new_intervals;
    });
    min_range(&intervals)
}
