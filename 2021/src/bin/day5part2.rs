use aoc_lib::_2d_int::{LineSegment, Point};
use aoc_lib::utils::parsing_input;

fn gcd<T>(mut v1: T, mut v2: T) -> T
where
    T: std::cmp::PartialEq + std::default::Default + std::ops::RemAssign + Copy,
{
    while v1 != Default::default() {
        v2 %= v1;
        std::mem::swap(&mut v1, &mut v2)
    }
    v2
}

fn iter_points(line: LineSegment<i32>) -> impl Iterator<Item = Point<i32>> + 'static {
    let delta = line.orientation();
    let steps = gcd(delta.x, delta.y).abs();

    let delta_step = if steps == 0 {
        Default::default()
    } else {
        delta / steps
    };

    (Default::default()..=steps).map(move |step| line.p0 + delta_step * step)
}

fn xed_points<I>(iter: I) -> impl Iterator<Item = Point<i32>> + 'static
where
    I: Iterator<Item = LineSegment<i32>>,
{
    let mut hmap = iter.flat_map(iter_points).fold(
        std::collections::HashMap::<_, usize>::new(),
        |mut hmap, item| {
            let count = hmap.remove(&item).unwrap_or_default() + 1;
            hmap.insert(item, count);
            hmap
        },
    );
    hmap.retain(|&_k, &mut v| v > 1);

    hmap.into_keys()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_xed_points_example() {
        let sequence = vec![
            LineSegment::new((0, 9), (5, 9)),
            LineSegment::new((8, 0), (0, 8)),
            LineSegment::new((9, 4), (3, 4)),
            LineSegment::new((2, 2), (2, 1)),
            LineSegment::new((7, 0), (7, 4)),
            LineSegment::new((6, 4), (2, 0)),
            LineSegment::new((0, 9), (2, 9)),
            LineSegment::new((3, 4), (1, 4)),
            LineSegment::new((0, 0), (8, 8)),
            LineSegment::new((5, 5), (8, 2)),
        ];

        let mut calc_result =
            xed_points(sequence.into_iter()).collect::<std::collections::HashSet<Point<i32>>>();

        for i in calc_result.iter() {
            println!("{:?}", i);
        }
        for p in vec![
            Point { x: 0, y: 9 },
            Point { x: 1, y: 9 },
            Point { x: 2, y: 2 },
            Point { x: 2, y: 9 },
            Point { x: 3, y: 4 },
            Point { x: 4, y: 4 },
            Point { x: 5, y: 3 },
            Point { x: 5, y: 5 },
            Point { x: 6, y: 4 },
            Point { x: 7, y: 1 },
            Point { x: 7, y: 3 },
            Point { x: 7, y: 4 },
        ]
        .into_iter()
        {
            assert!(calc_result.remove(&p));
        }
        assert!(calc_result.is_empty());
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, LineSegment<i32>>(stdin.lock());

    let xings = xed_points(parsed_inputs).count();
    println!("crossed points: {:?}", xings);
}
