use aoc_lib::utils::ArrayWrapper;

use core::str::FromStr;
use itertools::Itertools;
use std::collections::HashMap;
use std::io::BufRead;

struct Insertion {
    first: char,
    last: char,
    insert: char,
}

impl FromStr for Insertion {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 7 {
            return Err("wrong string length");
        }
        match s.chars().collect::<ArrayWrapper<char, 7>>() {
            ArrayWrapper([first, last, ' ', '-', '>', ' ', insert]) => Ok(Self {
                first,
                last,
                insert,
            }),
            _ => Err("wrong format"),
        }
    }
}

fn read_input<R: BufRead>(mut reader: R) -> Result<(String, Vec<Insertion>), &'static str> {
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer).map_err(|_| "bad read")?;

    let strings = buffer.split("\n\n").collect::<ArrayWrapper<&str, 2>>().0;

    let initial_seq = strings[0].trim();
    let insertions: Vec<_> = strings[1]
        .trim()
        .split('\n')
        .map(Insertion::from_str)
        .collect::<Result<_, _>>()?;

    Ok((initial_seq.to_string(), insertions))
}

type PairCounts = HashMap<(char, char), usize>;
type PolymerPairCountMap = HashMap<(char, char), PairCounts>;

fn new_pair_counts(s: &str) -> PairCounts {
    s.chars()
        .tuple_windows()
        .fold(PairCounts::new(), |mut pair_counts, pair| {
            let count_ref = pair_counts.entry(pair).or_insert(0);
            *count_ref += 1;

            pair_counts
        })
}

fn new_map<I: Iterator<Item = Insertion>>(insertions: I) -> PolymerPairCountMap {
    insertions
        .map(|insert| {
            let mut new = HashMap::new();
            new.insert((insert.first, insert.insert), 1);
            new.insert(
                (insert.insert, insert.last),
                1 + new
                    .get(&(insert.insert, insert.last))
                    .copied()
                    .unwrap_or_default(),
            );

            ((insert.first, insert.last), new)
        })
        .collect()
}

fn next_tier(insertion_map: &PolymerPairCountMap) -> PolymerPairCountMap {
    insertion_map
        .iter()
        .map(|(&k, pair_counts)| (k, apply_tier(pair_counts, insertion_map)))
        .collect()
}

fn apply_tier(pattern_counts: &PairCounts, insertion_map: &PolymerPairCountMap) -> PairCounts {
    let mut result = PairCounts::new();

    for (pair, &pair_count) in pattern_counts.iter() {
        if let Some(new_pair_counts) = insertion_map.get(pair) {
            for (other_pair, other_count) in new_pair_counts.iter() {
                let count_ref = result.entry(*other_pair).or_insert(0);
                *count_ref += other_count * pair_count;
            }
        } else {
            let count_ref = result.entry(*pair).or_insert(0);
            *count_ref += pair_count;
        }
    }

    result
}

fn polymerized_counts(
    template: &str,
    mut insertion_map: PolymerPairCountMap,
    iterations: u32,
) -> PairCounts {
    let bit_len = 32 - iterations.leading_zeros();

    let mut template = new_pair_counts(template);

    if iterations > 0 {
        for i in 0..(bit_len - 1) {
            if (iterations & (1 << i)) != 0 {
                template = apply_tier(&template, &insertion_map);
            }
            insertion_map = next_tier(&insertion_map);
        }
        template = apply_tier(&template, &insertion_map);
    }

    template
}

fn element_counts(pair_counts: &PairCounts) -> HashMap<char, usize> {
    let mut result = pair_counts
        .iter()
        .fold(HashMap::new(), |mut map, (pair, &count)| {
            for c in [pair.0, pair.1] {
                let count_ref = map.entry(c).or_insert(0);
                *count_ref += count;
            }
            map
        });

    for (_, count) in result.iter_mut() {
        *count = (*count / 2) + (*count % 2);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_polymerized_eg() {
        let template = "NNCB";
        let insertion_map = new_map(
            IntoIterator::into_iter([
                ['C', 'H', 'B'],
                ['H', 'H', 'N'],
                ['C', 'B', 'H'],
                ['N', 'H', 'C'],
                ['H', 'B', 'C'],
                ['H', 'C', 'B'],
                ['H', 'N', 'C'],
                ['N', 'N', 'C'],
                ['B', 'H', 'H'],
                ['N', 'C', 'B'],
                ['N', 'B', 'B'],
                ['B', 'N', 'B'],
                ['B', 'B', 'N'],
                ['B', 'C', 'B'],
                ['C', 'C', 'N'],
                ['C', 'N', 'C'],
            ])
            .map(|[c1, c2, c3]| Insertion {
                first: c1,
                last: c2,
                insert: c3,
            }),
        );

        let calc_result = polymerized_counts(template, insertion_map.clone(), 0);
        assert_eq!(calc_result, new_pair_counts("NNCB"), "polymerize 0 times");

        let calc_result = polymerized_counts(template, insertion_map.clone(), 1);
        assert_eq!(
            calc_result,
            new_pair_counts("NCNBCHB"),
            "polymerize 1 times"
        );

        let calc_result = polymerized_counts(template, insertion_map.clone(), 2);
        assert_eq!(
            calc_result,
            new_pair_counts("NBCCNBBBCBHCB"),
            "polymerize 2 times"
        );

        let calc_result = polymerized_counts(template, insertion_map.clone(), 3);
        assert_eq!(
            calc_result,
            new_pair_counts("NBBBCNCCNBBNBNBBCHBHHBCHB"),
            "polymerize 3 times"
        );

        let calc_result = polymerized_counts(template, insertion_map, 4);
        assert_eq!(
            calc_result,
            new_pair_counts("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"),
            "polymerize 4 times"
        );
    }

    #[test]
    fn test_count_polymer_elements_10_eg() {
        let template = "NNCB";
        let insertion_map = new_map(
            IntoIterator::into_iter([
                ['C', 'H', 'B'],
                ['H', 'H', 'N'],
                ['C', 'B', 'H'],
                ['N', 'H', 'C'],
                ['H', 'B', 'C'],
                ['H', 'C', 'B'],
                ['H', 'N', 'C'],
                ['N', 'N', 'C'],
                ['B', 'H', 'H'],
                ['N', 'C', 'B'],
                ['N', 'B', 'B'],
                ['B', 'N', 'B'],
                ['B', 'B', 'N'],
                ['B', 'C', 'B'],
                ['C', 'C', 'N'],
                ['C', 'N', 'C'],
            ])
            .map(|[c1, c2, c3]| Insertion {
                first: c1,
                last: c2,
                insert: c3,
            }),
        );

        let polymer = polymerized_counts(template, insertion_map, 10);
        let elems = element_counts(&polymer);
        let result =
            elems.values().cloned().max().unwrap() - elems.values().cloned().min().unwrap();
        assert_eq!(result, 1588)
    }

    #[test]
    fn test_count_polymer_elements_40_eg() {
        let template = "NNCB";
        let insertion_map = new_map(
            IntoIterator::into_iter([
                ['C', 'H', 'B'],
                ['H', 'H', 'N'],
                ['C', 'B', 'H'],
                ['N', 'H', 'C'],
                ['H', 'B', 'C'],
                ['H', 'C', 'B'],
                ['H', 'N', 'C'],
                ['N', 'N', 'C'],
                ['B', 'H', 'H'],
                ['N', 'C', 'B'],
                ['N', 'B', 'B'],
                ['B', 'N', 'B'],
                ['B', 'B', 'N'],
                ['B', 'C', 'B'],
                ['C', 'C', 'N'],
                ['C', 'N', 'C'],
            ])
            .map(|[c1, c2, c3]| Insertion {
                first: c1,
                last: c2,
                insert: c3,
            }),
        );

        let polymer = polymerized_counts(template, insertion_map, 40);
        let elems = element_counts(&polymer);
        let result =
            elems.values().cloned().max().unwrap() - elems.values().cloned().min().unwrap();
        assert_eq!(result, 2188189693529)
    }
}

fn main() {
    println!("Enter input sequence: ");
    let stdin = std::io::stdin();
    let (template, insertions) = read_input(stdin.lock()).unwrap();

    let polymer = polymerized_counts(&template, new_map(insertions.into_iter()), 40);
    let elems = element_counts(&polymer);
    println!("polymer element counts: {:?}", elems);
    println!(
        "result = {:?}",
        elems.values().cloned().max().unwrap() - elems.values().cloned().min().unwrap()
    );
}
