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

fn make_decoder(
    seg_groups: [SevenSegmentGroup; 10],
) -> Result<[SevenSegmentGroup; 10], &'static str> {
    const LENS_1478: [u32; 4] = [2, 3, 4, 7];
    let mut result: [Option<SevenSegmentGroup>; 10] = [Default::default(); 10];

    let (group_matches, others): (Vec<_>, Vec<_>) = IntoIterator::into_iter(seg_groups)
        .partition(|seg_group| LENS_1478.contains(&seg_group.0.count_ones()));
    for seg_group in group_matches.into_iter() {
        let map_value = match seg_group.0.count_ones() {
            2 => 1,
            3 => 7,
            4 => 4,
            7 => 8,
            _ => unreachable!("already confirmed that segment count is one of [2, 3, 4, 7]"),
        };
        if result[map_value].is_some() {
            return Err("encountered overlapping segment groups");
        }
        result[map_value] = Some(seg_group);
    }

    for seg_group in others.into_iter() {
        let bits = seg_group.0;
        let map_value = match (
            bits.count_ones(),
            (bits & result[1].ok_or("digit 1 format not found")?.0).count_ones(),
            (bits & result[4].ok_or("digit 4 format not found")?.0).count_ones(),
        ) {
            (5, 1, 2) => 2,
            (5, 2, 3) => 3,
            (5, 1, 3) => 5,
            (6, 2, 3) => 0,
            (6, 1, 3) => 6,
            (6, 2, 4) => 9,
            _ => return Err("invalid segment group intersections"),
        };

        if result[map_value].is_some() {
            return Err("encountered overlapping segment groups");
        }
        result[map_value] = Some(seg_group);
    }

    IntoIterator::into_iter(result)
        .map(|o| o.ok_or("uninitialized digit"))
        .collect()
}

fn decode_display<I>(
    decoder: &[SevenSegmentGroup; 10],
    digit_seg_groups: I,
) -> Result<u64, &'static str>
where
    I: Iterator<Item = SevenSegmentGroup>,
{
    let mut result = 0;
    for seg_group in digit_seg_groups {
        result = 10 * result
            + decoder
                .iter()
                .enumerate()
                .find_map(|(i, &seg_format)| (seg_format == seg_group).then(|| i as u64))
                .ok_or("segment group not found in decoder")?;
    }
    Ok(result)
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, InputWrapper>(stdin.lock());

    let result: u64 = parsed_inputs
        .map(|wrapper| (wrapper.0, wrapper.1))
        .map(|(segs, display)| {
            let decoder = make_decoder(segs).unwrap();

            decode_display(&decoder, IntoIterator::into_iter(display)).unwrap()
        })
        .sum();
    println!("counted 1478: {:?}", result);
}
