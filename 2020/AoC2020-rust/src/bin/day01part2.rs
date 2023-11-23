use std::cmp::Ord;
use std::ops::{Add, Sub};
use std::str::FromStr;
use std::io::BufRead;


fn parsing_input<R: BufRead, T: FromStr>(reader: R) -> impl Iterator<Item=T> {
	reader.lines()
		.filter_map(|r| r.ok())
		.filter_map(|s| s.parse::<T>().ok())
}

fn find_sum_triplet<T>(mut seq: Vec<T>, value: T) -> Option<(T, T, T)>
	where T: Copy + Ord + Add<Output=T> + Sub<Output=T>
{
	seq.sort_unstable();

	(0usize..seq.len())
		.flat_map(|i|
			(i+1..seq.len())
				.map(move |j| (i, j))
				.take_while(|(i, j)| seq[*i] + seq[*j] < value)
		)
		.find_map(|(i, j)|
			seq[j+1..]
				.binary_search(&(value-seq[i]-seq[j]))
				.ok()
				.map(|k| (seq[i], seq[j], seq[k+j+1]))
		)
}

fn main() {
	println!("Enter input sequence: ");
	let stdin = std::io::stdin();
	let parsed_inputs: Vec<u32> = parsing_input(stdin.lock()).collect();

	let vals = find_sum_triplet(parsed_inputs, 2020).unwrap();
	println!("triplet summing to 2020: {:?}, {:?}, {:?}", vals.0, vals.1, vals.2);
	println!("product: {:?}", vals.0 * vals.1 * vals.2);
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_find_sum_pair() {
		let sequence = vec![1721, 979, 366, 299, 675, 1456];
		assert_eq!(find_sum_triplet(sequence, 2020), Some((366, 675, 979)));
	}
}
