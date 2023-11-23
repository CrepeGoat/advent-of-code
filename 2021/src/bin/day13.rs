use aoc_lib::_2d_int::Point;
use aoc_lib::utils::ArrayWrapper;
use core::str::FromStr;
use std::io::BufRead;

use std::collections::{HashMap, HashSet};

fn read_input<R: BufRead>(
    mut reader: R,
) -> Result<(Vec<Point<i32>>, Vec<CartesianLine<i32>>), &'static str> {
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer).map_err(|_| "bad read")?;

    let strings = buffer.split("\n\n").collect::<ArrayWrapper<&str, 2>>().0;

    let points: Vec<Point<i32>> = strings[0]
        .trim()
        .split('\n')
        .map(Point::from_str)
        .collect::<Result<_, _>>()
        .map_err(|_| "bad point")?;

    let folds: Vec<CartesianLine<i32>> = strings[1]
        .trim()
        .split('\n')
        .map(|s| -> Result<CartesianLine<i32>, &'static str> {
            if !s.starts_with("fold along ") {
                return Err("bad fold format");
            }
            let s = &s[("fold along ".len())..];

            Ok(match &s[..2] {
                "x=" => CartesianLine::Vertical {
                    x: i32::from_str(&s[2..]).map_err(|_| "bad fold value")?,
                },
                "y=" => CartesianLine::Horizontal {
                    y: i32::from_str(&s[2..]).map_err(|_| "bad fold value")?,
                },
                _ => return Err("bad fold format"),
            })
        })
        .collect::<Result<_, _>>()
        .map_err(|_| "bad point")?;

    Ok((points, folds))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum CartesianLine<T> {
    Horizontal { y: T },
    Vertical { x: T },
}

fn fold_over(point: Point<i32>, line: CartesianLine<i32>) -> Point<i32> {
    use CartesianLine::*;

    match line {
        Horizontal { y: y_line } => Point {
            y: y_line - (y_line - point.y).abs(),
            ..point
        },
        Vertical { x: x_line } => Point {
            x: x_line - (x_line - point.x).abs(),
            ..point
        },
    }
}

fn unique_folded(points: &[Point<i32>], fold_lines: &[CartesianLine<i32>]) -> HashSet<Point<i32>> {
    let mut x_vals: HashMap<_, _> = points.iter().map(|p| (p.x, p.x)).collect();
    let mut y_vals: HashMap<_, _> = points.iter().map(|p| (p.y, p.y)).collect();

    use CartesianLine::*;
    for fold in fold_lines {
        match fold {
            Horizontal { y: y_line } => {
                for (_y_orig, y_map) in y_vals.iter_mut() {
                    *y_map = *y_line - (*y_line - *y_map).abs();
                }
            }
            Vertical { x: x_line } => {
                for (_x_orig, x_map) in x_vals.iter_mut() {
                    *x_map = *x_line - (*x_line - *x_map).abs();
                }
            }
        }
    }

    points
        .iter()
        .map(|&p| Point {
            x: *x_vals.get(&p.x).unwrap(),
            y: *y_vals.get(&p.y).unwrap(),
        })
        .collect()
}

fn print_dots<'a, C>(dots: C)
where
    C: 'a + IntoIterator<Item = &'a Point<i32>>,
{
    let mut paper = Vec::<Vec<bool>>::new();

    for dot in dots.into_iter() {
        let dot_x = dot.x as usize;
        let dot_y = dot.y as usize;

        if dot_y >= paper.len() {
            paper.resize(dot_y + 1, Default::default());
        }
        while dot_x >= paper[dot_y].len() {
            paper[dot_y].resize(dot_x + 1, Default::default())
        }

        paper[dot_y][dot_x] = true;
    }

    for line in paper {
        println!(
            "{}",
            line.iter()
                .map(|b| if *b { '#' } else { ' ' })
                .collect::<String>()
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_visible_points_example() {
        let points = vec![
            Point { x: 6, y: 10 },
            Point { x: 0, y: 14 },
            Point { x: 9, y: 10 },
            Point { x: 0, y: 3 },
            Point { x: 10, y: 4 },
            Point { x: 4, y: 11 },
            Point { x: 6, y: 0 },
            Point { x: 6, y: 12 },
            Point { x: 4, y: 1 },
            Point { x: 0, y: 13 },
            Point { x: 10, y: 12 },
            Point { x: 3, y: 4 },
            Point { x: 3, y: 0 },
            Point { x: 8, y: 4 },
            Point { x: 1, y: 10 },
            Point { x: 2, y: 14 },
            Point { x: 8, y: 10 },
            Point { x: 9, y: 0 },
        ];

        let fold_lines = vec![
            CartesianLine::Horizontal { y: 7 },
            CartesianLine::Vertical { x: 5 },
        ];

        let mut calc_result = unique_folded(&points, &fold_lines);

        for p in vec![
            Point { x: 0, y: 0 },
            Point { x: 0, y: 1 },
            Point { x: 0, y: 2 },
            Point { x: 0, y: 3 },
            Point { x: 0, y: 4 },
            Point { x: 1, y: 4 },
            Point { x: 2, y: 4 },
            Point { x: 3, y: 4 },
            Point { x: 4, y: 4 },
            Point { x: 4, y: 3 },
            Point { x: 4, y: 2 },
            Point { x: 4, y: 1 },
            Point { x: 4, y: 0 },
            Point { x: 3, y: 0 },
            Point { x: 2, y: 0 },
            Point { x: 1, y: 0 },
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
    let (points, folds) = read_input(stdin.lock()).unwrap();

    let dots = unique_folded(&points, &folds);
    println!("unique folded points: {:?}", dots.len());

    println!("dots:");
    print_dots(&dots);
}
