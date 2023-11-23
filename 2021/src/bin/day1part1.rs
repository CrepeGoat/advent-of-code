use itertools::Itertools;

use std::cmp::Ord;
use std::io::BufRead;
use std::ops::Sub;
use std::str::FromStr;

fn parsing_input<R: BufRead, T: FromStr>(reader: R) -> impl Iterator<Item = T> {
    reader
        .lines()
        .filter_map(|r| r.ok())
        .filter_map(|s| s.parse::<T>().ok())
}

fn count_diffs<I, T>(seq: I) -> usize
where
    T: Copy + Ord + Sub<Output = T> + Default,
    I: Iterator<Item = T>,
{
    seq.tuple_windows()
        .map(|(v1, v2)| v2 - v1)
        .filter(|v| v > &T::default())
        .count()
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input(stdin.lock());

    let diffs_count = count_diffs::<_, i32>(parsed_inputs);
    println!("diffs count: {:?}", diffs_count);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_diffs() {
        let sequence = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        assert_eq!(count_diffs(sequence.into_iter()), 7_usize);
    }
}
