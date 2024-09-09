#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use nom::{
    branch::alt,
    character::complete::{alpha1, digit1, line_ending},
    combinator::map,
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult,
};

fn parse_digit_line(input: &str) -> IResult<&str, String> {
    let omit = alt((line_ending, alpha1));
    let core = separated_list1(omit, digit1);
    let delimitor_pre = many0(alpha1);
    let delimitor_post = many0(alpha1);
    map(delimited(delimitor_pre, core, delimitor_post), |v| {
        v.concat()
    })(input)
}

fn main() {
    let res = parse_digit_line("a12a45");
    println!("Res: {:?}", res);
}

#[cfg(test)]
mod tests {

    use crate::parse_digit_line;

    #[test]
    fn can_parse_line() {
        assert_eq!(Ok(("", String::from("12"))), parse_digit_line("12a"));
        assert_eq!(Ok(("", String::from("12"))), parse_digit_line("a12"));
        assert_eq!(Ok(("", String::from("12"))), parse_digit_line("a12b"));
        assert_eq!(
            Ok(("", String::from("12345"))),
            parse_digit_line("a12b3c45d")
        );
    }
}
