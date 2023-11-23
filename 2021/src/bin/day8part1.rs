use aoc_lib::utils::parsing_input;

use core::str::FromStr;

fn map_seg_char(c: char) -> Result<u8, &'static str> {
    if !('a'..='g').contains(&c) {
        return Err("char out of range");
    }

    Ok(1 << ((c as u8) - b'a'))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, std::hash::Hash)]
struct SevenSegmentGroup(u8);

impl FromStr for SevenSegmentGroup {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut result = 0;
        for c in s.chars() {
            let char_bit = map_seg_char(c)?;
            if result & char_bit != 0 {
                return Err("encountered duplicate segment");
            }
            result |= char_bit;
        }

        Ok(Self(result))
    }
}

impl<const LEN: usize> std::iter::FromIterator<SevenSegmentGroup> for [SevenSegmentGroup; LEN] {
    fn from_iter<I: IntoIterator<Item = SevenSegmentGroup>>(iterable: I) -> Self {
        let mut result: Self = unsafe { std::mem::uninitialized() };

        let mut write_len = 0;
        for (i, item) in iterable.into_iter().enumerate() {
            unsafe {
                std::ptr::write(&mut result[i], item);
            }
            write_len += 1;
        }
        if write_len != LEN {
            panic!("wrong number of entries found; expected {:?}", LEN);
        }

        result
    }
}

struct InputWrapper([SevenSegmentGroup; 10], [SevenSegmentGroup; 4]);

#[derive(Debug)]
pub enum ParseInputError<T: FromStr>
where
    <T as FromStr>::Err: core::fmt::Debug,
{
    InnerType(<T as FromStr>::Err),
    IO(std::io::Error),
}

impl FromStr for InputWrapper {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split('|');

        let v1 = iter
            .next()
            .ok_or("no first group list")?
            .trim()
            .split_whitespace()
            .map(SevenSegmentGroup::from_str)
            .collect::<Result<[SevenSegmentGroup; 10], _>>()?;
        let v2 = iter
            .next()
            .ok_or("no first group list")?
            .trim()
            .split_whitespace()
            .map(SevenSegmentGroup::from_str)
            .collect::<Result<[SevenSegmentGroup; 4], _>>()?;

        Ok(InputWrapper(v1, v2))
    }
}

fn count_1478<I>(iter: I) -> usize
where
    I: Iterator<Item = InputWrapper>,
{
    iter.map(|w| {
        IntoIterator::into_iter(w.1)
            .filter_map(|seg| [2, 3, 4, 7].contains(&seg.0.count_ones()).then(|| seg))
            .count()
    })
    .sum()
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, InputWrapper>(stdin.lock());

    let result = count_1478(parsed_inputs);
    println!("counted 1478: {:?}", result);
}
