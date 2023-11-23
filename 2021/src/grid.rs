use itertools::Itertools;

use crate::utils::ArrayWrapper;
use std::io::BufRead;

pub type Grid<T, const ROW: usize, const COL: usize> = [[T; COL]; ROW];

pub fn adj4_coords<const ROW: usize, const COL: usize>(
    center: (usize, usize),
) -> impl 'static + Iterator<Item = (usize, usize)> {
    let mut result = Vec::new();

    if let Some(offset_0) = center.0.checked_sub(1) {
        result.push((offset_0, center.1));
    }
    if let Some(offset_1) = center.1.checked_sub(1) {
        result.push((center.0, offset_1));
    }
    if center.0 < ROW - 1 {
        result.push((center.0 + 1, center.1));
    }
    if center.1 < COL - 1 {
        result.push((center.0, center.1 + 1));
    }

    result.into_iter()
}

pub fn adj8_coords<const ROW: usize, const COL: usize>(
    center: (usize, usize),
) -> impl 'static + Iterator<Item = (usize, usize)> {
    let mut i_vals = vec![center.0];
    let mut j_vals = vec![center.1];

    if let Some(offset_i) = center.0.checked_sub(1) {
        i_vals.push(offset_i);
    }
    if let Some(offset_j) = center.1.checked_sub(1) {
        j_vals.push(offset_j);
    }
    if center.0 < ROW - 1 {
        i_vals.push(center.0 + 1);
    }
    if center.1 < COL - 1 {
        j_vals.push(center.1 + 1);
    }

    i_vals
        .into_iter()
        .cartesian_product(j_vals.into_iter())
        .skip(1)
}

pub fn read_input<R: BufRead, const ROW: usize, const COL: usize>(
    reader: R,
) -> Result<Grid<u32, ROW, COL>, &'static str> {
    reader
        .lines()
        .map(|r| r.map_err(|_| "bad line read"))
        .map(|r| -> Result<_, &'static str> {
            r.and_then(|s| {
                s.chars()
                    .map(|c| c.to_digit(10).ok_or("bad digit"))
                    .collect::<Result<ArrayWrapper<_, COL>, _>>()
                    .map(From::from)
            })
        })
        .collect::<Result<ArrayWrapper<_, ROW>, _>>()
        .map(From::from)
}
