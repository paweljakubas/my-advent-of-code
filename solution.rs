#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use nom::{
    character::complete::{alpha1, digit1},
    combinator::map,
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult,
};

fn parse_digit_line(input: &str) -> IResult<&str, String> {
    let core = separated_list1(alpha1, digit1);
    let delimitor_pre = many0(alpha1);
    let delimitor_post = many0(alpha1);
    map(delimited(delimitor_pre, core, delimitor_post), |v| {
        v.concat()
    })(input)
}

fn part1(_input: &str) -> u32 {
    1
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

    use crate::{parse_digit_line, part1, part2};

    type TestResult<T> = Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn can_parse_line() {
        assert_eq!(Ok(("", String::from("12"))), parse_digit_line("12a"));
    }

    #[test]
    fn sample_test1() -> TestResult<()> {
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/1/sample1.txt")?;
        assert_eq!(1, part1(&input));
        Ok(())
    }

    #[test]
    fn sample_test2() -> TestResult<()> {
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/1/sample1.txt")?;
        assert_eq!(2, part2(&input));
        Ok(())
    }
}
