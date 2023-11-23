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


fn parse_input(input_seq: &str) -> Result<Vec<Direction>, ParseDirectionError> {
	input_seq.split('\n').map(|s: &str| Direction::from_str(&s)).collect()
}


fn calc_path_offset(path: Vec<Direction>, init_heading: Complex<i64>)
-> (Complex<i64>, Complex<i64>)
{
	use Direction::{North, South, East, West, Left, Right, Forward};
	type Coords = Complex<i64>;

	let mut heading = init_heading;
	let mut position = Coords::new(0, 0);

	for instruction in path {
		match instruction {
			North(dist) => {position += dist as i64 * Coords::new(0, 1);},
			South(dist) => {position += dist as i64 * Coords::new(0, -1);},
			East(dist) => {position += dist as i64 * Coords::new(1, 0);},
			West(dist) => {position += dist as i64 * Coords::new(-1, 0);},
			Left(rot) => {heading *= Coords::new(0, 1).powu((rot as u32) / 90);},
			Right(rot) => {heading *= Coords::new(0, -1).powu((rot as u32) / 90);},
			Forward(dist) => {position += dist as i64 * heading;},
		}
	}

	(position, heading)
}

fn main() {
	let mut io_buffer = String::new();
	let mut path = Vec::<Direction>::new();
	
	println!("Enter directions:");
	while
		std::io::stdin().read_line(&mut io_buffer).is_ok()
		&& {io_buffer = io_buffer.trim().to_string(); !io_buffer.is_empty()}
	{
		println!("{:?}", io_buffer);
		path.push(Direction::from_str(&io_buffer.as_str()).unwrap());
		io_buffer.clear();
	}

	let (position, _heading) = calc_path_offset(path, Complex::<i64>::new(1, 0));
	println!("coords: {:?}", (position.re, position.im));
	println!("distance: {:?}", position.l1_norm());
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_input() {
		use Direction::{North, Right, Forward};

		let eg_text = "F10\nN3\nF7\nR90\nF11";
		assert_eq!(
			parse_input(eg_text).unwrap(),
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
	fn test_calc_path_offset() {
		use Direction::{North, Right, Forward};

		let eg_instructions = vec![
			Forward(10u64),
			North(3u64),
			Forward(7u64),
			Right(90u16),
			Forward(11u64),
		];
		assert_eq!(
			calc_path_offset(eg_instructions, Complex::<i64>::new(1, 0)),
			(Complex::<i64>::new(17, -8), Complex::<i64>::new(0, -1)),
		)
	}
}