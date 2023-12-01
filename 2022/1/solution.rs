#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! nom = "7.1.3"
//! ```

use nom::{
    character::complete::{digit1, line_ending},
    combinator::{map, map_res},
    multi::{many1,separated_list1},
    IResult,
};
use std::str::FromStr;
use std::env;
use std::fs;

#[derive(Debug, Eq, PartialEq)]
pub struct Items{content: Vec<u32>}

impl Items {
    fn parse(input: &str) -> IResult<&str, Self> {
        let parse_number = map_res(digit1, u32::from_str);

        // Parse numbers, separated by new line
        let parse_items = separated_list1(line_ending, parse_number);

        // If the parse succeeded, put those items into Items
        map(parse_items, |items| Items{content: items})(input)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ItemsList {content: Vec<Items>}

impl ItemsList {
    fn parse(input: &str) -> IResult<&str, Self> {
        // Parse list of items, separated by new lines
        let parse_items_list = separated_list1(many1(line_ending), Items::parse);

        // If the parse succeeded, put those items into ItemsList
        map(parse_items_list, |items| ItemsList {content: items})(input)
    }
}

fn main() {
    let working_dir =
        env::current_dir().
        unwrap().
        into_os_string().
        into_string().
        unwrap();
    let file_path = working_dir + "/inputTest1.txt";
    let input = fs::read_to_string(file_path).unwrap();
    let (_rest, items_list) = ItemsList::parse(&input).unwrap();
    let part1 =
        items_list.content.
        iter().
        map (|i| i.content).
        collect::<Vec<Vec<u32>>>();
    println!("{:?}", part1);
}
