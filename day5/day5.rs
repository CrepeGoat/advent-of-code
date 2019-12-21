use std::cmp::min;
use std::ops::{RangeBounds, Bound::*};
use std::vec::Vec;


#[derive(Debug, Clone, Copy)]
struct Digits(u32);

impl Digits {
	const NO_OF_DIGITS: u8 = 9;

	fn digits<R: RangeBounds<u8>>(&self, index: R) -> u32 {
		let lbound = match index.start_bound() {
			Unbounded => 0,
			Included(&n) => n,
			Excluded(&n) => n+1
		};
		let ubound = match index.end_bound() {
			Unbounded => Self::NO_OF_DIGITS,
			Included(&n) => min(Self::NO_OF_DIGITS, n+1),
			Excluded(&n) => min(Self::NO_OF_DIGITS, n),
		};

		if lbound >= ubound {
			0_u32
		} else {
			(self.0 / 10_u32.pow(lbound.into()))
			% 10_u32.pow((ubound-lbound).into())
		}
	}
}

//-----------------------------------------------------------------------------

fn parse_code_string(output: &mut Vec<i32>, input: &str) {
	for word in input.split(",") {
		output.push(word.trim().parse::<i32>().expect("invalid input string"));
	}
}

//-----------------------------------------------------------------------------

fn exec_code(code: &mut Vec<usize>) {
	let mut code_pos = 0usize;

	while code_pos < code.len() {
		let op = code[code_pos];

		match op {
			99usize => break,
			1usize => {
				let arg1pos = code[code_pos+1];
				let arg2pos = code[code_pos+2];
				let respos = code[code_pos+3];

				code[respos] = code[arg1pos] + code[arg2pos];
				code_pos += 4;
			},
			2usize => {
				let arg1pos = code[code_pos+1];
				let arg2pos = code[code_pos+2];
				let respos = code[code_pos+3];

				code[respos] = code[arg1pos] * code[arg2pos];
				code_pos += 4;
			},
			_ => panic!("invalid opcode {:?}", op),
		}
	}
}

fn find_noun_verb(code: &Vec<usize>, expected_result: usize) -> [usize; 2] {
	for noun in 0usize..=99usize {
		for verb in 0usize..=99usize {
			let mut new_code = code.clone();
			new_code[1] = noun;
			new_code[2] = verb;

			exec_code(&mut new_code);

			if new_code[0] == expected_result {
				return [noun, verb];
			}
		}
	}

	panic!("no solution found");
}

//------------------------------------------------------------------
fn print_code(code: &Vec<usize>) {
	let iter = code.iter();

	/*match iter.next() {
		Some(expr) => print!("{:?}", expr),
		None => {},
	}
	*/
	for ch in iter {
		print!("{:?},", ch);
	}

	println!("");
}

fn main() {
	let d = Digits(123456780);

	println!("{:?}", d.digits(1..=2))

	/*
	println!("Enter program code below:");
	let mut code = Vec::<usize>::new();
	let mut buffer = String::new();
	std::io::stdin().read_line(&mut buffer).expect("invalid code");
	parse_code_string(&mut code, &buffer);
	//exec_code(&mut code);
	buffer.clear();
	
	println!("Enter expected result at address 0:");
	std::io::stdin().read_line(&mut buffer).expect("no expected result found");
	let expected_result = buffer.trim().parse::<usize>().expect("invalid expected result");
	let desired_input = find_noun_verb(&code, expected_result);

	println!("{:?}{:?}", desired_input[0], desired_input[1]);
	*/
}