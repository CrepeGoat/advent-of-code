use core::fmt::Debug;
use core::ops::Index;
use core::ops::IndexMut;
use std::io::BufRead;

use core::str::FromStr;

#[derive(Debug)]
pub enum ParseInputError<T: FromStr>
where
    <T as FromStr>::Err: Debug,
{
    InnerType(<T as FromStr>::Err),
    IO(std::io::Error),
}

pub fn parse_input<R: BufRead, T: FromStr>(mut reader: R) -> Result<Vec<T>, ParseInputError<T>>
where
    <T as FromStr>::Err: Debug,
{
    let mut buffer = String::new();
    reader.read_line(&mut buffer).map_err(ParseInputError::IO)?;

    buffer
        .split(',')
        .map(T::from_str)
        .map(|r| r.map_err(ParseInputError::InnerType))
        .collect()
}

#[derive(Debug, Copy, Clone, Hash)]
struct RollingBuffer<T, const LEN: usize>([T; LEN], usize);

impl<T, const LEN: usize> RollingBuffer<T, LEN> {
    fn new(array: [T; LEN]) -> Self {
        Self(array, 0)
    }

    fn roll(&mut self, index: usize) {
        self.1 = (self.1 + index) % LEN;
    }
}

impl<T, const LEN: usize> Index<usize> for RollingBuffer<T, LEN> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[(index + self.1) % LEN]
    }
}

impl<T, const LEN: usize> IndexMut<usize> for RollingBuffer<T, LEN> {
    fn index_mut(&mut self, index: usize) -> &mut <Self as Index<usize>>::Output {
        &mut self.0[(index + self.1) % LEN]
    }
}

fn simulate_lanternfish<I>(iter: I) -> impl Iterator<Item = RollingBuffer<usize, 9>>
where
    I: Iterator<Item = usize>,
{
    let mut counter = RollingBuffer([0; 9], 0);
    for i in iter {
        counter[i] += 1;
    }

    std::iter::once(counter).chain(itertools::unfold((), move |_| {
        counter[7] += counter[0];
        counter.roll(1);

        Some(counter)
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_simfish_example1() {
        let sequence = vec![3, 4, 3, 1, 2];
        let calc_result = simulate_lanternfish(sequence.into_iter())
            .map(|cntr| cntr.0.iter().sum())
            .take(18 + 1)
            .collect::<Vec<usize>>();

        assert_eq!(
            calc_result,
            vec![5, 5, 6, 7, 9, 10, 10, 10, 10, 11, 12, 15, 17, 19, 20, 20, 21, 22, 26]
        );
    }

    #[test]
    fn test_count_simfish_example2() {
        let sequence = vec![3, 4, 3, 1, 2];
        let calc_result: usize = simulate_lanternfish(sequence.into_iter())
            .nth(80)
            .unwrap()
            .0
            .iter()
            .sum();

        assert_eq!(calc_result, 5934);
    }

    #[test]
    fn test_count_simfish_example3() {
        let sequence = vec![3, 4, 3, 1, 2];
        let calc_result: usize = simulate_lanternfish(sequence.into_iter())
            .nth(256)
            .unwrap()
            .0
            .iter()
            .sum();

        assert_eq!(calc_result, 26984457539);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parse_input::<_, usize>(stdin.lock()).unwrap();

    let n = 256;
    let count_simfish: usize = simulate_lanternfish(parsed_inputs.into_iter())
        .nth(n)
        .unwrap()
        .0
        .iter()
        .sum();
    println!("{:?}th epoch fish count: {:?}", n, count_simfish);
}
