use counter::Counter;
use std::fs::File;
use std::io::Read;
const CARDS: &str = "23456789TJQKA";
const EXAMPLE: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
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
    println!("Testing hand_value...");
    assert_eq!(hand_value_part1("AAAAA"), 0x6CCCCC);
    assert_eq!(hand_value_part1("23456"), 0x1234);
    println!("Testing part1...");
    assert_eq!(part1(EXAMPLE), 6440);

    println!("Testing hand_value_part2...");
    // Without Joker
    assert_eq!(hand_value_part2("AAAAA"), 0x6CCCCC);
    assert_eq!(hand_value_part2("23456"), 0x1234);

    // With Joker cards
    // Five of a kind
    assert_eq!(hand_value_part2("AAAAJ"), 0x6CCCC9);
    assert_eq!(hand_value_part2("AAAJJ"), 0x6CCC99);
    assert_eq!(hand_value_part2("AAJJJ"), 0x6CC999);
    assert_eq!(hand_value_part2("AJJJJ"), 0x6C9999);
    // Four of a kind
    assert_eq!(hand_value_part2("AJJJ2"), 0x5C9990);
    assert_eq!(hand_value_part2("JAJJ2"), 0x59C990);
    assert_eq!(hand_value_part2("JJAJ2"), 0x599C90);
    assert_eq!(hand_value_part2("AJAJ2"), 0x5C9C90);
    assert_eq!(hand_value_part2("AJAA2"), 0x5C9CC0);
    // Full house
    assert_eq!(hand_value_part2("JAA22"), 0x49CC00);
    assert_eq!(hand_value_part2("AAJ22"), 0x4CC900);
    assert_eq!(hand_value_part2("AAKKJ"), 0x4CCBB9);
    // Three of a kind
    assert_eq!(hand_value_part2("AAJ23"), 0x3CC901);
    assert_eq!(hand_value_part2("AJJ23"), 0x3C9901);
    // One pair
    assert_eq!(hand_value_part2("2345J"), 0x101239);
    assert_eq!(hand_value_part2("234J5"), 0x101293);
    assert_eq!(hand_value_part2("23J45"), 0x101923);
    assert_eq!(hand_value_part2("2J345"), 0x109123);
    assert_eq!(hand_value_part2("J2345"), 0x190123);

    // Examples inputs
    // "32T3K is still the only one pair; it doesn't contain any jokers, so its strength doesn't increase."
    assert_eq!(hand_value_part2("32T3K"), 0x11081B);
    // "KK677 is now the only two pair, making it the second-weakest hand."
    assert_eq!(hand_value_part2("KK677"), 0x2BB455);
    // "T55J5, KTJJT, and QQQJA are now all four of a kind! T55J5 gets rank 3, QQQJA gets rank 4, and KTJJT gets rank 5."
    assert_eq!(hand_value_part2("T55J5"), 0x583393);
    assert_eq!(hand_value_part2("KTJJT"), 0x5B8998);
    assert_eq!(hand_value_part2("QQQJA"), 0x5AAA9C);
    // Inputs
    assert_eq!(hand_value_part2("JJJA8"), 0x5999C6);
    assert_eq!(hand_value_part2("JJJJJ"), 0x699999);
    assert_eq!(hand_value_part2("T7JJJ"), 0x585999);
    assert_eq!(hand_value_part2("JJJ2J"), 0x699909);
    assert_eq!(hand_value_part2("332JJ"), 0x511099);

    println!("Testing part2...");
    assert_eq!(part2(EXAMPLE), 5905);
}

fn card_type_part1(h: &str) -> u32 {
    // 0 for "highest card", 1 for "one pair",
    // up to 6 for a "five of a kind".
    let counter: Counter<char, u32> = Counter::init(h.chars());
    match (
        counter.most_common_ordered()[0],
        counter.most_common_ordered().get(1),
    ) {
        ((_, 5), None) => 6,         // Five of a kind
        ((_, 4), _) => 5,            // Four of a kind
        ((_, 3), Some((_, 2))) => 4, // Full house
        ((_, 3), _) => 3,            // Three of a kind
        ((_, 2), Some((_, 2))) => 2, // Two pairs
        ((_, 2), _) => 1,            // One pair
        _ => 0,                      // Highest cardJ
    }
}

fn hand_value_part1(h: &str) -> u32 {
    // each card is represented by an hexadecimal value with 6 symbols: IJKLMN
    // `I``, the 1st digit represents the "type" of the hand, cf card_type_part1
    // `J` the 2nd digit represents the value of the 1st card: 0 for a "2", 1 for a "3",
    //and so on up to C (=12) for the ace.
    // `K` represents the 2nd card with the same principle
    // And so on until `N`, which represents the last card
    // So the highest hand "AAAAA" is represented by 0x6CCCCC,
    // And "23456" is represented by 0x001234

    // Computing the type
    let mut card_value = card_type_part1(h);
    for c in h.chars() {
        card_value *= 16;
        card_value += CARDS.find(c).unwrap() as u32;
    }
    card_value
}

fn part1(s: &str) -> u32 {
    let hands = parse_input(s);
    let mut hands_values: Vec<(u32, &&str, &u32)> = hands
        .iter()
        .map(|(h, bid)| (hand_value_part1(&h), h, bid))
        .collect();
    hands_values.sort();
    // hands_values.reverse();
    let mut count: u32 = 0;
    for (i, (_, _, bid)) in hands_values.iter().enumerate() {
        // println!("i={i}, bid={bid}");
        count += (i as u32 + 1) * **bid;
    }

    count
}

fn card_type_part2(h: &str) -> u32 {
    // 0 for "highest card", 1 for "one pair",
    // up to 6 for a "five of a kind".
    // If we're here, 'h' contains at least one Joker.
    let mut counter: Counter<char, u32> = Counter::init(h.chars());
    // If the most common card is the Joker, the Joker can be replaced by the 2nd most common card to get
    // the best hand.
    // If not, the Jokers should be replaced by the most common card
    // There's no point in replacing the Joker by anything else than the most common card.
    // For instance if we have AAKJ2 => the best type we can have is a "three of a kind", it's better than "two pairs"
    // The only "composed" hand type we can have is a "full house", if we have AAKKJ for instance.
    let num_jokers = counter.get(&'J').unwrap().clone();
    counter.remove(&'J');
    match (
        counter.most_common_ordered().get(0),
        counter.most_common_ordered().get(1),
        num_jokers,
    ) {
        // Five of a kind with Jokers only:
        (_, _, 5) => 6,
        // Five of a kind, KKKKJ, KKKJJ, KKJJJ, or KJJJJ for instance:
        (Some((_, n)), _, m) if n + m == 5 => 6,
        // Four of a kind
        (Some((_, n)), _, m) if n + m == 4 => 5,
        (Some((_, n)), Some((_, third_most_common)), m) if n + m == 3 => {
            match third_most_common {
                2 => 4, // Full house
                1 => 3, // Three of a kind
                _ => panic!("Not supposed to happen??"),
            }
        }
        // Two pairs is not possible
        // One pair
        (Some((_, 1)), _, 1) => 1, // "Highest card" is not possible
        _ => panic!("Should not happen"),
    }
}

fn hand_value_part2(h: &str) -> u32 {
    // each card is represented by an hexadecimal value with 6 symbols: IJKLMN
    // `I``, the 1st digit represents the "type" of the hand, cf card_type_part1
    // `J` the 2nd digit represents the value of the 1st card: 0 for a "2", 1 for a "3",
    //and so on up to C (=12) for the ace.
    // `K` represents the 2nd card with the same principle
    // And so on until `N`, which represents the last card
    // So the highest hand "AAAAA" is represented by 0x6CCCCC,
    // And "23456" is represented by 0x001234

    // Computing the type
    let mut card_value = if !h.contains('J') {
        card_type_part1(h)
    } else {
        card_type_part2(h)
    };
    for c in h.chars() {
        card_value *= 16;
        card_value += CARDS.find(c).unwrap() as u32;
    }
    card_value
}

fn part2(s: &str) -> u32 {
    let hands = parse_input(s);
    let _: Vec<_> = hands
        .iter()
        .map(|(h, v)| check_hand_value_equal(h))
        .collect();
    let mut hands_values: Vec<(u32, &&str, &u32)> = hands
        .iter()
        .map(|(h, bid)| (hand_value_part2(&h), h, bid))
        .collect();
    hands_values.sort();
    let mut count: u32 = 0;
    for (i, (_, h, bid)) in hands_values.iter().enumerate() {
        println!("i={i}, h={h}, bid={bid}");
        count += (i as u32 + 1) * **bid;
    }

    count
}
fn every_combination(hand: &str) -> Vec<String> {
    // Create every combination by replacing 'J' by every card
    let mut combinations: Vec<_> = vec![hand.to_string()];

    while let Some(j_index) = combinations.last().unwrap().find('J') {
        let mut new_combinations = Vec::new();

        for digit in CARDS.chars() {
            for existing in &combinations {
                let mut new_string = existing.clone();
                new_string.replace_range(j_index..j_index + 1, &digit.to_string());
                new_combinations.push(new_string);
            }
        }

        combinations = new_combinations;
    }

    combinations
}

fn check_hand_value_equal(hand: &str) {
    // Function written just to check that the hand value computed for part 2 is correct...
    if !hand.contains('J') {
        return ();
    }
    println!("Checking card {hand}...");
    let combs = every_combination(hand);
    let combs_types = combs.iter().map(|s| card_type_part1(&s));
    let binding = combs_types.collect::<Vec<u32>>();
    let best_type_all_combinations = binding.iter().max().unwrap();
    // let val_patterns = hand_value_part2(&hand);
    // Checking that the type is the same
    let card_type = card_type_part2(&hand);
    assert_eq!(best_type_all_combinations, &card_type);

    // Checking the rest of the value is the same
    let value = hand_value_part2(hand);
    let value_without_type = value % 0x100000;
    print!("value={value:#x}, type={card_type:#x}, value without type = {value_without_type:#x}");

    let val_part1 = hand_value_part1(hand);
    let valp1_without_type = val_part1 % 0x100000;
    assert_eq!(valp1_without_type, value_without_type)
}

fn parse_line(line: &str) -> (&str, u32) {
    let split: Vec<&str> = line.split(' ').collect();
    (split[0], split[1].parse().unwrap())
}

fn parse_input(s: &str) -> Vec<(&str, u32)> {
    s.trim().lines().map(parse_line).collect()
}
