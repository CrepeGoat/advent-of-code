use std::io::{BufRead, BufReader};
use std::str::FromStr;
use std::vec::Vec;
use std::num::ParseIntError;

use num_complex::Complex;


#[derive(Debug)]
enum ParseDirectionError {
	InstructionType,
	ParseInt(ParseIntError),
	RotationValue,
}

#[derive(Debug, PartialEq, Eq)]
enum Direction {
	North(u64),
	South(u64),
	East(u64),
	West(u64),
	Left(u16),
	Right(u16),
	Forward(u64),
}

impl FromStr for Direction {
	type Err = ParseDirectionError;

	fn from_str(text: &str) -> Result<Self, Self::Err> {
		use Direction::{North, South, East, West, Left, Right, Forward};
		use ParseDirectionError as ErrType;

		fn parse_int<I: FromStr<Err=ParseIntError>>(instr_text: &str) -> Result<I, ErrType> {
			instr_text.parse::<I>().map_err(ErrType::ParseInt)
		}

		match text.chars().next().ok_or(ErrType::InstructionType)?.to_ascii_uppercase() {
			'N' => Ok(North(parse_int::<u64>(&text[1..])?)),
			'S' => Ok(South(parse_int::<u64>(&text[1..])?)),
			'E' => Ok(East(parse_int::<u64>(&text[1..])?)),
			'W' => Ok(West(parse_int::<u64>(&text[1..])?)),
			'L' => Ok(Left(match parse_int::<u16>(&text[1..])? {
				90 => Ok(90),
				180 => Ok(180),
				270 => Ok(270),
				_val => Err(Self::Err::RotationValue),
			}?)),
			'R' => Ok(Right(match parse_int::<u16>(&text[1..])? {
				90 => Ok(90),
				180 => Ok(180),
				270 => Ok(270),
				_val => Err(Self::Err::RotationValue),
			}?)),
			'F' => Ok(Forward(parse_int::<u64>(&text[1..])?)),
			_ => Err(Self::Err::InstructionType),
		}
	}
}


fn parsing_input<R: BufRead>(reader: R) -> impl Iterator<Item=Direction> {
	reader.lines()
		.filter_map(|r| r.ok())
		.filter_map(|s| s.parse::<Direction>().ok())
}


fn increment_path(instruction: Direction, position: Complex<i64>, waypoint: Complex<i64>)
-> (Complex<i64>, Complex<i64>)
{
	use Direction::{North, South, East, West, Left, Right, Forward};

	match instruction {
		North(dist) => (position, waypoint + (dist as i64 * Complex::<i64>::new(0, 1))),
		South(dist) => (position, waypoint + (dist as i64 * Complex::<i64>::new(0, -1))),
		East(dist) => (position, waypoint + (dist as i64 * Complex::<i64>::new(1, 0))),
		West(dist) => (position, waypoint + (dist as i64 * Complex::<i64>::new(-1, 0))),
		Left(rot) => (position, waypoint * (Complex::<i64>::new(0, 1).powu((rot as u32) / 90))),
		Right(rot) => (position, waypoint * (Complex::<i64>::new(0, -1).powu((rot as u32) / 90))),
		Forward(dist) => (position + (dist as i64 * waypoint), waypoint),
	}
}


fn calc_path_result<I>(iter: I, init_waypt: Complex<i64>)
-> (Complex<i64>, Complex<i64>)
	where I: Iterator<Item=Direction>
{
	iter.fold(
		(Complex::<i64>::new(0, 0), init_waypt),
		|pos_waypt, dir| increment_path(dir, pos_waypt.0, pos_waypt.1),
	)
}

fn main() {
	let stdin = std::io::stdin();
	let iter_parsed_inputs = parsing_input(stdin.lock());

	let (position, _waypoint) = calc_path_result(
		iter_parsed_inputs, Complex::<i64>::new(10, 1)
	);
	println!("coords: {:?}", (position.re, position.im));
	println!("distance: {:?}", position.l1_norm());
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parsing_input() {
		use Direction::{North, Right, Forward};

		let eg_text = "F10\nN3\nF7\nR90\nF11";
		assert_eq!(
			parsing_input(BufReader::new(eg_text.as_bytes())).collect::<Vec<_>>(),
			vec![
				Forward(10u64),
				North(3u64),
				Forward(7u64),
				Right(90u16),
				Forward(11u64),
			],
		)
	}

	#[test]
	fn test_calc_path_result() {
		use Direction::{North, Right, Forward};

		let eg_instructions = vec![
			Forward(10u64),
			North(3u64),
			Forward(7u64),
			Right(90u16),
			Forward(11u64),
		];
		assert_eq!(
			calc_path_result(eg_instructions.into_iter(), Complex::<i64>::new(10, 1)),
			(Complex::<i64>::new(214, -72), Complex::<i64>::new(4, -10)),
		)
	}
}