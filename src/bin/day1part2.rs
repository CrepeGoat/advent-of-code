use itertools::Itertools;

use std::cmp::Ord;
use std::io::BufRead;
use std::ops::{Add, Sub};
use std::str::FromStr;

fn parsing_input<R: BufRead, T: FromStr>(reader: R) -> impl Iterator<Item = T> {
    reader
        .lines()
        .filter_map(|r| r.ok())
        .filter_map(|s| s.parse::<T>().ok())
}

fn count_diffs<I, T, U>(seq: I) -> usize
where
    T: Copy + Add<Output = T> + Sub<Output = U>,
    U: Copy + Ord + Default,
    I: Iterator<Item = T>,
{
    seq.tuple_windows()
        .map(|(v1, v2, v3)| v1 + v2 + v3)
        .tuple_windows()
        .map(|(v1, v2)| v2 - v1)
        .filter(|v| v > &U::default())
        .count()
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input(stdin.lock());

    let diffs_count = count_diffs::<_, i32, _>(parsed_inputs);
    println!("diffs count: {:?}", diffs_count);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_sum_pair() {
        let sequence = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(count_diffs(sequence.into_iter()), 5_usize);
    }
}
