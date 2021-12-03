use aoc_lib::utils::parsing_input;

use std::convert::{From, Into};
use std::str::FromStr;

#[derive(Debug)]
enum Direction {
    Forward,
    Backward,
    Upward,
    Downward,
}

impl FromStr for Direction {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "forward" => Ok(Self::Forward),
            "backward" => Ok(Self::Backward),
            "upward" => Ok(Self::Upward),
            "downward" => Ok(Self::Downward),
            _ => Err("invalid string"),
        }
    }
}

struct Merged<T1, T2>(T1, T2);

impl<T1, T2> Merged<T1, T2> {
    fn new(t1: T1, t2: T2) -> Self {
        Self(t1, t2)
    }
}

impl<T1, T2> From<(T1, T2)> for Merged<T1, T2> {
    fn from(value: (T1, T2)) -> Self {
        Self::new(value.0, value.1)
    }
}

impl<T1, T2> Into<(T1, T2)> for Merged<T1, T2> {
    fn into(self: Self) -> (T1, T2) {
        (self.0, self.1)
    }
}

enum MergedErr<E1, E2> {
    Err1(E1),
    Err2(E2),
} 

impl<T1: FromStr, T2: FromStr> FromStr for Merged<T1, T2> {
    type Err = MergedErr<<T1 as FromStr>::Err, <T2 as FromStr>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split_iter = s.split(" ");

        let t1 = split_iter
            .next().unwrap()
            .parse::<T1>()
            .or_else(|e1| Err(Self::Err::Err1(e1)))?;

        let t2 = split_iter
            .next().unwrap()
            .parse::<T2>()
            .or_else(|e2| Err(Self::Err::Err2(e2)))?;

        Ok(Self::new(t1, t2))
    }
}

fn track_loc<I>(seq: I) -> (i32, i32)
where
    I: Iterator<Item = (Direction, i32)>,
{
    seq.fold((0, 0), |(down_up, fwd_bwd), (direction, dist)| {
        match direction {
            Direction::Forward => (down_up, fwd_bwd + dist),
            Direction::Backward => (down_up, fwd_bwd - dist),
            Direction::Upward => (down_up - dist, fwd_bwd),
            Direction::Downward => (down_up + dist, fwd_bwd),
        }
    })
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input(stdin.lock()).map(|m: Merged<_, _>| m.into());

    let (depth, dist) = track_loc(parsed_inputs);
    println!("depth: {:?}\ndist: {:?}", depth, dist);
    println!("product: {:?}", depth * dist);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_track_loc() {
        let sequence = vec![
            (Direction::Forward, 5),
            (Direction::Downward, 5),
            (Direction::Forward, 8),
            (Direction::Upward, 3),
            (Direction::Downward, 8),
            (Direction::Forward, 2),
        ];
        assert_eq!(track_loc(sequence.into_iter()), (10, 15));
    }
}
