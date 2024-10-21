#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, space0, space1},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq, Hash, Eq)]
enum Color {
    Red(u32),
    Green(u32),
    Blue(u32),
}

fn parse_red(input: &str) -> IResult<&str, Color> {
    let parser = delimited(
        space0::<&str, _>,
        tuple((digit1, space1, tag("red"))),
        space0::<&str, _>,
    );
    map(parser, |pair| {
        let num = (pair.0).to_string().parse::<u32>().unwrap();
        Color::Red(num)
    })(input)
}

fn parse_green(input: &str) -> IResult<&str, Color> {
    let parser = delimited(
        space0::<&str, _>,
        tuple((digit1, space1, tag("green"))),
        space0::<&str, _>,
    );
    map(parser, |pair| {
        let num = (pair.0).to_string().parse::<u32>().unwrap();
        Color::Green(num)
    })(input)
}

fn parse_blue(input: &str) -> IResult<&str, Color> {
    let parser = delimited(
        space0::<&str, _>,
        tuple((digit1, space1, tag("blue"))),
        space0::<&str, _>,
    );
    map(parser, |pair| {
        let num = (pair.0).to_string().parse::<u32>().unwrap();
        Color::Blue(num)
    })(input)
}

fn parse_color(input: &str) -> IResult<&str, Color> {
    alt((parse_green, parse_red, parse_blue))(input)
}

#[derive(Debug, PartialEq)]
struct Cube {
    cube: HashMap<String, u32>,
}

fn parse_cube(input: &str) -> IResult<&str, Cube> {
    let parser = separated_list1(tag(","), parse_color);
    map(parser, |colors| {
        let mut map = HashMap::new();
        for color in colors {
            match color {
                Color::Red(num) => {
                    map.insert("red".to_string(), num);
                }
                Color::Green(num) => {
                    map.insert("green".to_string(), num);
                }
                Color::Blue(num) => {
                    map.insert("blue".to_string(), num);
                }
            }
        }
        Cube { cube: map }
    })(input)
}

#[derive(Debug, PartialEq)]
struct Game {
    id: u32,
    cubes: Vec<Cube>,
}

fn parse_game(input: &str) -> IResult<&str, Game> {
    let num_parser = preceded(tag("Game "), digit1);
    let cubes_parser = separated_list1(tag(";"), parse_cube);
    let parser = tuple((num_parser, tag(":"), cubes_parser));
    map(parser, |(num, _, cs)| {
        let id = num.parse::<u32>().unwrap();
        let mut cubes = Vec::new();
        for cube in cs {
            cubes.push(cube);
        }
        Game { id, cubes }
    })(input)
}

fn games_parser(input: &str) -> IResult<&str, Vec<Game>> {
    separated_list1(line_ending, parse_game)(input)
}

fn check_color(color: &str, query_cube: &Cube, max_cube: &Cube) -> bool {
    match query_cube.cube.get(color) {
        Some(val) => val <= max_cube.cube.get(color).unwrap(),
        None => true,
    }
}

fn cmp_each_game(query_cube: &Cube, max_cube: &Cube) -> bool {
    let colors = max_cube.cube.keys();
    colors.fold(true, |acc, color| {
        acc && check_color(&color, &query_cube, &max_cube)
    })
}

fn is_each_valid(game: &Game) -> bool {
    let limit = Cube {
        cube: HashMap::from([
            ("blue".to_string(), 14),
            ("red".to_string(), 12),
            ("green".to_string(), 13),
        ]),
    };
    game.cubes
        .iter()
        .fold(true, |acc, cube| acc && cmp_each_game(cube, &limit))
}

fn part1(input: &str) -> u32 {
    if let Ok((_, games)) = games_parser(input) {
        println!("games: {:?}", games);
        games
            .iter()
            .filter(|&game| is_each_valid(&game))
            .map(|game| game.id)
            .sum()
    } else {
        0
    }
}
fn part2(_input: &str) -> u32 {
    2
}

type CustomizedResult<T> = Result<T, Box<dyn std::error::Error>>;

fn main() -> CustomizedResult<()> {
    let args: Vec<String> = std::env::args().collect();
    match &args[..] {
        [_name, filename, part] => {
            let input = std::fs::read_to_string(filename)?;
            if part == "part1" {
                println!("{}", part1(&input));
                Ok(())
            } else if part == "part2" {
                println!("{}", part2(&input));
                Ok(())
            } else {
                println!("Error: Either part1 or part2 are expected");
                Ok(())
            }
        }
        _ => {
            println!("Error: both filename and part1/part2 are expected");
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        games_parser, parse_color, parse_cube, parse_game, part1, part2, Color, Cube, Game,
    };
    use std::collections::HashMap;

    type TestResult<T> = Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn can_parse_color() {
        assert_eq!(Ok(("", Color::Green(12))), parse_color("12 green"));
        assert_eq!(Ok(("", Color::Green(12))), parse_color(" 12  green "));
        assert_eq!(Ok(("", Color::Red(121))), parse_color("121 red"));
        assert_eq!(Ok(("", Color::Blue(2))), parse_color("2 blue "));
    }

    #[test]
    fn can_parse_cube1() {
        let map = HashMap::from([("green".to_string(), 12), ("red".to_string(), 1)]);
        assert_eq!(Ok(("", Cube { cube: map })), parse_cube("12 green, 1 red"));
    }

    #[test]
    fn can_parse_cube2() {
        let map = HashMap::from([
            ("green".to_string(), 3),
            ("blue".to_string(), 4),
            ("red".to_string(), 1),
        ]);
        assert_eq!(
            Ok(("", Cube { cube: map })),
            parse_cube("3 green, 4 blue, 1 red")
        );
    }

    #[test]
    fn can_parse_game1() {
        let vec = vec![
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 3),
                    ("blue".to_string(), 1),
                    ("red".to_string(), 6),
                ]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 2),
                    ("blue".to_string(), 2),
                    ("red".to_string(), 1),
                ]),
            },
        ];
        assert_eq!(
            Ok(("", Game { id: 5, cubes: vec })),
            parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
        );
    }

    #[test]
    fn can_parse_game2() {
        let vec = vec![
            Cube {
                cube: HashMap::from([("blue".to_string(), 3), ("red".to_string(), 4)]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 2),
                    ("blue".to_string(), 6),
                    ("red".to_string(), 1),
                ]),
            },
            Cube {
                cube: HashMap::from([("green".to_string(), 2)]),
            },
        ];
        assert_eq!(
            Ok(("", Game { id: 1, cubes: vec })),
            parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
        );
    }

    #[test]
    fn can_parse_sample1() -> TestResult<()> {
        let game1 = vec![
            Cube {
                cube: HashMap::from([("blue".to_string(), 3), ("red".to_string(), 4)]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 2),
                    ("blue".to_string(), 6),
                    ("red".to_string(), 1),
                ]),
            },
            Cube {
                cube: HashMap::from([("green".to_string(), 2)]),
            },
        ];
        let game2 = vec![
            Cube {
                cube: HashMap::from([("blue".to_string(), 1), ("green".to_string(), 2)]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 3),
                    ("blue".to_string(), 4),
                    ("red".to_string(), 1),
                ]),
            },
            Cube {
                cube: HashMap::from([("green".to_string(), 1), ("blue".to_string(), 1)]),
            },
        ];
        let game3 = vec![
            Cube {
                cube: HashMap::from([
                    ("blue".to_string(), 6),
                    ("green".to_string(), 8),
                    ("red".to_string(), 20),
                ]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 13),
                    ("blue".to_string(), 5),
                    ("red".to_string(), 4),
                ]),
            },
            Cube {
                cube: HashMap::from([("green".to_string(), 5), ("red".to_string(), 1)]),
            },
        ];
        let game4 = vec![
            Cube {
                cube: HashMap::from([
                    ("blue".to_string(), 6),
                    ("green".to_string(), 1),
                    ("red".to_string(), 3),
                ]),
            },
            Cube {
                cube: HashMap::from([("green".to_string(), 3), ("red".to_string(), 6)]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 3),
                    ("blue".to_string(), 15),
                    ("red".to_string(), 14),
                ]),
            },
        ];
        let game5 = vec![
            Cube {
                cube: HashMap::from([
                    ("blue".to_string(), 1),
                    ("green".to_string(), 3),
                    ("red".to_string(), 6),
                ]),
            },
            Cube {
                cube: HashMap::from([
                    ("green".to_string(), 2),
                    ("blue".to_string(), 2),
                    ("red".to_string(), 1),
                ]),
            },
        ];
        let games = vec![
            Game {
                id: 1,
                cubes: game1,
            },
            Game {
                id: 2,
                cubes: game2,
            },
            Game {
                id: 3,
                cubes: game3,
            },
            Game {
                id: 4,
                cubes: game4,
            },
            Game {
                id: 5,
                cubes: game5,
            },
        ];
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/2/sample1.txt")?;
        assert_eq!(Ok(("\n", games)), games_parser(&input));
        Ok(())
    }

    #[test]
    fn sample_test1() -> TestResult<()> {
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/2/sample1.txt")?;
        assert_eq!(8, part1(&input));
        Ok(())
    }

    #[test]
    fn sample_test2() -> TestResult<()> {
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/2/sample1.txt")?;
        assert_eq!(2, part2(&input));
        Ok(())
    }
}
