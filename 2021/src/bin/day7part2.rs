use core::fmt::Debug;
use core::str::FromStr;
use std::io::BufRead;

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
    buffer.pop();

    buffer
        .split(',')
        .map(T::from_str)
        .map(|r| r.map_err(ParseInputError::InnerType))
        .collect()
}

fn crab_loc_counts<I>(locs: I) -> Vec<usize>
where
    I: Iterator<Item = usize>,
{
    locs.fold(Vec::new(), |mut loc_cntr, i| {
        if i >= loc_cntr.len() {
            loc_cntr.resize_with(i + 1, Default::default);
        }
        loc_cntr[i] += 1;
        loc_cntr
    })
}

fn crab_fuel_cost(loc_counts: &[usize], center_loc: usize) -> usize {
    let center_loc = center_loc as isize;
    loc_counts
        .iter()
        .enumerate()
        .map(|(i, count)| (((i as isize - center_loc).abs() as usize), count))
        .fold(0, |sum, (i, &count)| sum + ((i * (i + 1)) * count) / 2)
}

fn min_crab_fuel_cost(loc_counts: &[usize]) -> (usize, usize) {
    let sum: usize = loc_counts
        .iter()
        .enumerate()
        .map(|(i, &count)| i * count)
        .sum();
    let count: usize = loc_counts.iter().sum();
    let i_mean = sum / count;

    let score1 = crab_fuel_cost(loc_counts, i_mean);
    let score2 = crab_fuel_cost(loc_counts, i_mean + 1);

    if score1 <= score2 {
        (i_mean, score1)
    } else {
        (i_mean + 1, score2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_min_crab_fuel_example1() {
        let sequence = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        let crab_locs = crab_loc_counts(sequence.into_iter());
        let calc_result = min_crab_fuel_cost(&crab_locs);
        assert_eq!(calc_result, (5, 168));
    }

    #[test]
    fn test_min_crab_fuel_example2() {
        let sequence = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        let crab_locs = crab_loc_counts(sequence.into_iter());
        let calc_result = crab_fuel_cost(&crab_locs, 2);
        assert_eq!(calc_result, 206);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parse_input::<_, usize>(stdin.lock()).unwrap();

    let crab_locs = crab_loc_counts(parsed_inputs.into_iter());
    let crab_fuel = min_crab_fuel_cost(&crab_locs);
    println!("min crab fuel: {:?}", crab_fuel);
}
