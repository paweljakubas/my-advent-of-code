#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use nom::{
    character::complete::{alpha1, digit1, newline},
    combinator::{map, map_res},
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult,
};

fn digit_line_parser(input: &str) -> IResult<&str, String> {
    let core = separated_list1(alpha1, digit1);
    let delimitor_pre = many0(alpha1);
    let delimitor_post = many0(alpha1);
    map(delimited(delimitor_pre, core, delimitor_post), |v| {
        v.concat()
    })(input)
}

fn two_chars_parser(input: &str) -> IResult<&str, (char, char)> {
    map(digit_line_parser, |chars| {
        let cs: Vec<_> = chars.chars().collect();
        let len = cs.len();
        (cs[0], cs[len - 1])
    })(input)
}

fn two_digits_parser(input: &str) -> IResult<&str, u32> {
    map_res(two_chars_parser, |(c1, c2)| {
        let d1 = c1.to_string().parse::<u32>().unwrap();
        let d2 = c2.to_string().parse::<u32>().unwrap();
        Ok::<u32, std::num::ParseIntError>(d1 * 10 + d2)
    })(input)
}

fn lines_parser(input: &str) -> IResult<&str, Vec<u32>> {
    separated_list1(newline, two_digits_parser)(input)
}

type CustomizedResult<T> = Result<T, Box<dyn std::error::Error>>;

fn main() -> CustomizedResult<()> {
    let args: Vec<String> = std::env::args().collect();
    match &args[..] {
        [_name, filename, part] => {
            let input = std::fs::read_to_string(filename)?;
            if part == "part1" {
                if let Ok((_, ints)) = lines_parser(&input) {
                    println!("{}", ints.iter().sum::<u32>());
                }
                Ok(())
            } else if part == "part2" {
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

    use crate::{digit_line_parser, lines_parser, two_chars_parser, two_digits_parser};

    type TestResult<T> = Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn can_parse_line() {
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("12a"));
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("a12"));
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("a12b"));
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("12"));
        assert_eq!(Ok(("", String::from("1"))), digit_line_parser("1"));
        assert_eq!(
            Ok(("", String::from("12"))),
            digit_line_parser("1aaaaaaaaaaaaaaa2")
        );
        assert_eq!(
            Ok(("", String::from("12345"))),
            digit_line_parser("a12b3c45d")
        );
        assert_eq!(Ok(("", String::from("3"))), digit_line_parser("fx3"));
        assert_eq!(
            Ok(("", String::from("8"))),
            digit_line_parser("8nrbjbpjpnineseven")
        );
    }

    #[test]
    fn can_two_chars_parser() {
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("12a"));
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("a12"));
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("a12b"));
        assert_eq!(Ok(("\n", ('1', '1'))), two_chars_parser("a1b\n"));
        assert_eq!(Ok(("", ('3', '3'))), two_chars_parser("fx3"));
        assert_eq!(Ok(("", ('8', '8'))), two_chars_parser("8nrbjbpjpnineseven"));
    }

    #[test]
    fn can_parse_two_u32() {
        assert_eq!(Ok(("", 12)), two_digits_parser("12a"));
        assert_eq!(Ok(("", 12)), two_digits_parser("a12"));
        assert_eq!(Ok(("", 12)), two_digits_parser("a12b"));
        assert_eq!(Ok(("", 15)), two_digits_parser("a12b3c45d"));
        assert_eq!(Ok(("\n", 15)), two_digits_parser("a12b3c45d\n"));
        assert_eq!(Ok(("", 33)), two_digits_parser("fx3"));
        assert_eq!(Ok(("", 88)), two_digits_parser("8nrbjbpjpnineseven"));
    }

    #[test]
    fn can_parse_lines() {
        assert_eq!(Ok(("", vec![13])), lines_parser("13a"));

        let mut str = "12a\naaaa22zzz";
        assert_eq!(Ok(("", vec![12, 22])), lines_parser(str));

        str = "a12b3c45d\n1iii0000iii2a\na12b3c40d\nzzzzz1ababaab";
        assert_eq!(Ok(("", vec![15, 12, 10, 11])), lines_parser(str));

        str = "fx3\n8nrbjbpjpnineseven";
        assert_eq!(Ok(("", vec![33, 88])), lines_parser(str));

        str = "3\n8\n9\n";
        assert_eq!(Ok(("\n", vec![33, 88, 99])), lines_parser(str));
    }

    #[test]
    fn sample_test1() -> TestResult<()> {
        let input =
            std::fs::read_to_string("/home/pawel/Work/my-advent-of-code/2023/1/sample1.txt")?;
        assert_eq!(Ok(("\n", vec![12, 38, 15, 77])), lines_parser(&input));
        Ok(())
    }
}
