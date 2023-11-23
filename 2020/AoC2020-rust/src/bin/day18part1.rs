use std::io::BufRead;
use std::ops::{Add, Mul};


fn could_be_expr<T>(s: &str) -> bool {
	let mut count_parens = 0isize;

	for c in s.chars() {
		match c {
			'(' => count_parens += 1,
			')' => {
				count_parens -= 1;
				if count_parens < 0 {
					return false
				}
			},
			_ => (),
		}
	}
	count_parens == 0
}

fn parse_expr<T>(s: &str) -> Result<T, ()>
where T: Copy + Add<Output = T> + Mul<Output = T> + std::str::FromStr
{
	// Short-circuit obviously wrong expressions
	if !could_be_expr::<T>(s) {
		return Err(());
	}

	// Expression in parentheses
	if &s[..1] == "(" && &s[s.len()-1..] == ")" {
		if let Ok(val) = parse_expr(&s[1..s.len()-1]) {
			return Ok(val);
		}
	}

	// Literal number
	if let Ok(val) = s.parse::<T>() {
		return Ok(val);
	}

	// Nested operations
	let op_len = " _ ".len();
	if s.len() < 2 + op_len {
		return Err(());
	}
	for (i, op_char) in (1..(s.len() - op_len)).rev()
		.filter_map(|i| match &s[i..i+op_len] {
			" + " => Some((i, '+')),
			" * " => Some((i, '*')),
			_ => None
		})
	{
		if let Ok(lhs) = parse_expr::<T>(&s[..i]) {
			if let Ok(rhs) = parse_expr::<T>(&s[i+op_len..]) {
				return Ok(match op_char {
					'+' => lhs + rhs,
					'*' => lhs * rhs,
					_ => unreachable!(),
				});
			}
		}
	}

	// Imparseable input
	Err(())
}


fn main() {
	let stream = std::io::BufReader::new(std::io::stdin());	
	let result = stream.lines().map(|r| r.unwrap())
		.map(|s| parse_expr::<u64>(&s[..]))
		.try_fold(0, |x, xi| xi.map(|y| x+y)).unwrap();

	println!("{:?}", result);
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn parse0() {
		assert_eq!(parse_expr::<u64>("1 + 2 * 3 + 4 * 5 + 6"), Ok(71));
	}

	#[test]
	fn parse1() {
		assert_eq!(parse_expr::<u64>("1 + (2 * 3) + (4 * (5 + 6))"), Ok(51));
	}

	#[test]
	fn parse2() {
		assert_eq!(parse_expr::<u64>("2 * 3 + (4 * 5)"), Ok(26));
	}

	#[test]
	fn parse3() {
		assert_eq!(parse_expr::<u64>("5 + (8 * 3 + 9 + 3 * 4 * 3)"), Ok(437));
	}

	#[test]
	fn parse4() {
		assert_eq!(
			parse_expr::<u64>("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
			Ok(12240),
		);
	}

	#[test]
	fn parse5() {
		assert_eq!(
			parse_expr::<u64>("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"),
			Ok(13632),
		);
	}

	#[test]
	fn parse6() {
		assert_eq!(
			parse_expr::<u64>("((3) * (2))"),
			Ok(6),
		);
	}
}
