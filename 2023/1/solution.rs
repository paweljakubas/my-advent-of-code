#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use nom::{
    branch::alt,
    character::complete::{alpha1, digit1, line_ending, newline},
    combinator::{map, map_res},
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult,
};

fn digit_line_parser(input: &str) -> IResult<&str, String> {
    let omit = alt((line_ending, alpha1));
    let core = separated_list1(omit, digit1);
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

fn main() {
    let res = lines_parser("a12a45\n");
    println!("Res: {:?}", res);
}

#[cfg(test)]
mod tests {

    use crate::{digit_line_parser, lines_parser, two_chars_parser, two_digits_parser};

    #[test]
    fn can_parse_line() {
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("12a"));
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("a12"));
        assert_eq!(Ok(("", String::from("12"))), digit_line_parser("a12b"));
        assert_eq!(
            Ok(("", String::from("12345"))),
            digit_line_parser("a12b3c45d")
        );
    }

    #[test]
    fn can_two_chars_parser() {
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("12a"));
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("a12"));
        assert_eq!(Ok(("", ('1', '2'))), two_chars_parser("a12b"));
        assert_eq!(Ok(("", ('1', '5'))), two_chars_parser("a12b3c45d"));
    }

    #[test]
    fn can_parse_two_u32() {
        assert_eq!(Ok(("", 12)), two_digits_parser("12a"));
        assert_eq!(Ok(("", 12)), two_digits_parser("a12"));
        assert_eq!(Ok(("", 12)), two_digits_parser("a12b"));
        assert_eq!(Ok(("", 15)), two_digits_parser("a12b3c45d"));
    }

    #[test]
    fn can_parse_lines() {
        assert_eq!(Ok(("", vec![13])), lines_parser("13a"));
        let mut str = "12a\naaaa22zzz";
        assert_eq!(Ok(("", vec![12, 22])), lines_parser(str));
        str = "a12b3c45d\n12a\na12b3c40d\nzzzzz1ababaab";
        assert_eq!(Ok(("", vec![15, 12, 10, 11])), lines_parser(str));
    }
}
