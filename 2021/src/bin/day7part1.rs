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

fn crab_fuel<I>(locs: I) -> Vec<usize>
where
    I: Iterator<Item = usize>,
{
    let loc_counts = locs.fold(Vec::new(), |mut loc_cntr, i| {
        if i >= loc_cntr.len() {
            loc_cntr.resize_with(i + 1, Default::default);
        }
        loc_cntr[i] += 1;
        loc_cntr
    });
    let n: usize = loc_counts.len();

    let prefix_sums: Vec<usize> = {
        let mut sum = 0;
        std::iter::once(&0)
            .chain(loc_counts.iter())
            .map(move |item| {
                sum += item;
                sum
            })
    }
    .collect();
    let whole_sum = prefix_sums[n];

    let zero_index_score: usize = loc_counts
        .iter()
        .enumerate()
        .map(|(i, &count)| i * count)
        .sum();

    {
        let mut sum = zero_index_score + whole_sum;
        prefix_sums.into_iter().map(move |item| {
            sum += 2 * item;
            sum -= whole_sum;
            sum
        })
    }
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_min_crab_fuel_example1() {
        let sequence = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        assert_eq!(
            crab_fuel(sequence.into_iter()),
            vec![49, 41, 37, 39, 41, 45, 49, 53, 59, 65, 71, 77, 83, 89, 95, 103, 111, 121]
        );
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parse_input::<_, usize>(stdin.lock()).unwrap();

    let crab_fuel: usize = crab_fuel(parsed_inputs.into_iter())
        .into_iter()
        .min()
        .expect("no items in input");
    println!("min crab fuel: {:?}", crab_fuel);
}
