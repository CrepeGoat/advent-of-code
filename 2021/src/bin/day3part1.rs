use aoc_lib::utils::parsing_input;
use aoc_lib::vectorized::Vectorized;

use std::convert::Into;

fn calculate_gamma_epsilon<I, const N: usize>(iter: I) -> (u32, u32)
where
    I: Iterator<Item = Vectorized<bool, N>>,
{
    let (count, sums) = iter
        .map(|v| v.map(|&b| b.into()))
        .fold((0, Vectorized([0_u32; N])), |(count, sum), x| {
            (count + 1, sum + x)
        });
    let half_count = count / 2;
    let gamma = sums.map(|x| x > &half_count);
    let epsilon = gamma.map(|x| !x);

    (gamma.into(), epsilon.into())
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, Vectorized<bool, 12>>(stdin.lock());

    let (gamma, epsilon) = calculate_gamma_epsilon(parsed_inputs);
    println!("gamma: {:?}\nepsilon: {:?}", gamma, epsilon);
    println!("product: {:?}", gamma * epsilon);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gamma_example() {
        let sequence: Vec<Vectorized<_, 5>> = vec![
            Vectorized([0, 0, 1, 0, 0]),
            Vectorized([1, 1, 1, 1, 0]),
            Vectorized([1, 0, 1, 1, 0]),
            Vectorized([1, 0, 1, 1, 1]),
            Vectorized([1, 0, 1, 0, 1]),
            Vectorized([0, 1, 1, 1, 1]),
            Vectorized([0, 0, 1, 1, 1]),
            Vectorized([1, 1, 1, 0, 0]),
            Vectorized([1, 0, 0, 0, 0]),
            Vectorized([1, 1, 0, 0, 1]),
            Vectorized([0, 0, 0, 1, 0]),
            Vectorized([0, 1, 0, 1, 0]),
        ];
        assert_eq!(
            calculate_gamma_epsilon(sequence.into_iter().map(|v| v.map(|&i| i != 0))),
            (0b10110, 0b1001)
        );
    }
}
