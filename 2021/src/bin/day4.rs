use core::str::FromStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::BufRead;

type Matrix<T> = Vec<Vec<T>>;
type AnyError = Box<dyn Error>;

fn read_input<R: BufRead, T>(reader: R) -> Result<(Vec<T>, Vec<Matrix<T>>), AnyError>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: 'static + Error,
{
    let mut lines = reader.lines().peekable();

    let result1 = lines
        .next()
        .ok_or("no lines in input")??
        .split(',')
        .map(T::from_str)
        .collect::<Result<_, _>>()?;

    let mut result2 = Vec::new();
    while lines.next().is_some() && lines.peek().is_some() {
        result2.push(
            (&mut lines)
                .take(5)
                .map(|s| {
                    s.map_err(AnyError::from)?
                        .trim()
                        .split_whitespace()
                        .map(T::from_str)
                        .map(|r| r.map_err(AnyError::from))
                        .collect::<Result<_, _>>()
                })
                .collect::<Result<_, _>>()?,
        );
    }

    Ok((result1, result2))
}

fn first_bingo_winner_score<ITEMS, BOARDS, T, U, const ROW: usize, const COL: usize>(
    items: ITEMS,
    boards: BOARDS,
) -> Option<U>
where
    T: Copy
        + std::hash::Hash
        + std::cmp::Eq
        + std::default::Default
        + std::ops::Add<T, Output = T>
        + std::ops::Mul<T, Output = U>,
    ITEMS: Iterator<Item = T>,
    BOARDS: Iterator<Item = Matrix<T>>,
{
    let mut board_states: Vec<_> = boards
        .map(|board| {
            let mut vals_lookups: HashMap<T, _> = HashMap::new();
            let mut rows_lookups: Vec<HashSet<T>> = Vec::new();
            let mut cols_lookups: Vec<HashSet<T>> = Vec::new();

            for (i, row) in board.iter().enumerate().take(ROW) {
                for (j, cell) in row.iter().enumerate().take(COL) {
                    vals_lookups.insert(*cell, (i, j));
                }
            }
            for row in board.iter().take(ROW) {
                rows_lookups.push(row.iter().copied().collect());
            }
            for j in 0..COL {
                cols_lookups.push(board.iter().map(|row| row[j]).collect());
            }

            (vals_lookups, rows_lookups, cols_lookups)
        })
        .collect();

    for item in items {
        for (vals, rows, cols) in board_states.iter_mut() {
            if let Some((i, j)) = vals.remove(&item) {
                let row = &mut rows[i];
                assert!(row.remove(&item));
                if row.is_empty() {
                    return Some(item * vals.keys().fold(Default::default(), |sum, &x| x + sum));
                }

                let col = &mut cols[j];
                assert!(col.remove(&item));
                if col.is_empty() {
                    return Some(item * vals.keys().fold(Default::default(), |sum, &x| x + sum));
                }
            }
        }
    }
    None
}

fn last_bingo_winner_score<ITEMS, BOARDS, T, U, const ROW: usize, const COL: usize>(
    items: ITEMS,
    boards: BOARDS,
) -> Option<U>
where
    T: Copy
        + std::hash::Hash
        + std::cmp::Eq
        + std::default::Default
        + std::ops::Add<T, Output = T>
        + std::ops::Mul<T, Output = U>,
    ITEMS: Iterator<Item = T>,
    BOARDS: Iterator<Item = Matrix<T>>,
{
    let mut board_states: Vec<_> = boards
        .map(|board| {
            let mut vals_lookups: HashMap<T, _> = HashMap::new();
            let mut rows_lookups: Vec<HashSet<T>> = Vec::new();
            let mut cols_lookups: Vec<HashSet<T>> = Vec::new();

            for (i, row) in board.iter().enumerate().take(ROW) {
                for (j, cell) in row.iter().enumerate().take(COL) {
                    vals_lookups.insert(*cell, (i, j));
                }
            }
            for row in board.iter().take(ROW) {
                rows_lookups.push(row.iter().copied().collect());
            }
            for j in 0..COL {
                cols_lookups.push(board.iter().map(|row| row[j]).collect());
            }

            (true, (vals_lookups, rows_lookups, cols_lookups))
        })
        .collect();

    let mut result = None;

    for item in items {
        for (state, (vals, rows, cols)) in board_states.iter_mut().filter(|s| s.0) {
            if let Some((i, j)) = vals.remove(&item) {
                let row = &mut rows[i];
                assert!(row.remove(&item));
                if row.is_empty() {
                    result = Some(item * vals.keys().fold(Default::default(), |sum, &x| x + sum));
                    *state = false;
                }

                let col = &mut cols[j];
                assert!(col.remove(&item));
                if col.is_empty() {
                    result = Some(item * vals.keys().fold(Default::default(), |sum, &x| x + sum));
                    *state = false;
                }
            }
        }
    }
    result
}

fn main() {
    println!("Enter input: ");
    let stdin = std::io::stdin();

    let (items, boards) = read_input::<_, usize>(stdin.lock()).unwrap();
    //println!("items: {:?}\nboards: {:?}", items, boards);

    let score = last_bingo_winner_score::<_, _, _, _, 5, 5>(items.into_iter(), boards.into_iter());
    println!("score: {:?}", score);
}
