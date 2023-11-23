use std::cmp::{Ord, Ordering};
use std::ops::Add;
use std::str::FromStr;
use std::io::BufRead;


fn parsing_input<R: BufRead, T: FromStr>(reader: R) -> impl Iterator<Item=T> {
	reader.lines()
		.filter_map(|r| r.ok())
		.filter_map(|s| s.parse::<T>().ok())
}

fn find_sum_pair<T>(mut seq: Vec<T>, value: T) -> Option<(T, T)>
	where T: Copy + Ord + Add<Output=T>
{
	seq.sort_unstable();
	let mut i1 = 0usize;
	let mut i2 = seq.len()-1;

	while i1 < i2 {
		match (seq[i1] + seq[i2]).cmp(&value) {
			Ordering::Less => i1 += 1,  // sum is too small -> increase lower number
			Ordering::Greater => i2 -= 1,  // sum is too big -> decrease higher number
			Ordering::Equal => {return Some((seq[i1], seq[i2]))},  // found it!
		};
	}
	None
}

fn main() {
	println!("Enter input sequence: ");
	let stdin = std::io::stdin();
	let parsed_inputs: Vec<u32> = parsing_input(stdin.lock()).collect();

	let sum_pair = find_sum_pair(parsed_inputs, 2020).unwrap();
	println!("pair summing to 2020: {:?}, {:?}", sum_pair.0, sum_pair.1);
	println!("product: {:?}", sum_pair.0 * sum_pair.1);
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_find_sum_pair() {
		let sequence = vec![1721, 979, 366, 299, 675, 1456];
		assert_eq!(find_sum_pair(sequence, 2020), Some((299, 1721)));
	}
}
