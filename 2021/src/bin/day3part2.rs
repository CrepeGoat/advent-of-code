use aoc_lib::utils::parsing_input;
use aoc_lib::vectorized::Vectorized;

fn calculate_gas<const N: usize, const MAJORITY: bool>(
    vec: &[Vectorized<bool, N>],
) -> Result<u32, String> {
    let mut filt_vec: Vec<_> = vec.to_vec();

    for i in 0..N {
        let (count, sum) = filt_vec
            .iter()
            .map(|v| u32::from(v.0[i]))
            .fold((0, 0), |(c, s), x| (c + 1, s + x));
        let filt_bit = (2 * sum >= count) == MAJORITY;

        filt_vec = filt_vec
            .into_iter()
            .filter(|v| v.0[i] == filt_bit)
            .collect();

        if filt_vec.len() == 1 {
            break;
        }
    }

    if filt_vec.len() != 1 {
        return Err(format!(
            "did not reduce to single remaining value, {:?} values remaining",
            filt_vec.len()
        ));
    }

    Ok(filt_vec.pop().unwrap().into())
}

// fn calculate_co2scrub;

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs: Vec<Vectorized<bool, 12>> = parsing_input(stdin.lock()).collect();

    let oxygen = calculate_gas::<12, true>(&parsed_inputs).unwrap();
    println!("oxygen: {:?}", oxygen);
    let co2 = calculate_gas::<12, false>(&parsed_inputs).unwrap();
    println!("CO2: {:?}", co2);
    println!("product: {:?}", oxygen * co2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gamma_example() {
        let sequence: Vec<Vectorized<bool, 5>> = vec![
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
        ]
        .into_iter()
        .map(|v| v.map(|&i| i != 0))
        .collect();

        assert_eq!(calculate_gas::<5, true>(&sequence), Ok(23));
        assert_eq!(calculate_gas::<5, false>(&sequence), Ok(10));
    }
}
