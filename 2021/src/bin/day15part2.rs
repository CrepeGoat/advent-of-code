use aoc_lib::grid::{adj4_coords, read_input, Grid};
use core::cmp::Reverse;
use std::collections::BinaryHeap;

fn index_wrapped<const ROW: usize, const COL: usize>(
    grid: &Grid<u32, ROW, COL>,
    index: (usize, usize),
) -> u32 {
    ((grid[index.0 % ROW][index.1 % COL] + ((index.0 / ROW) + (index.1 / COL)) as u32) - 1) % 9 + 1
}

fn path_risk<const ROW: usize, const COL: usize, const WROW: usize, const WCOL: usize>(
    grid: &Grid<u32, ROW, COL>,
) -> u32 {
    assert!(COL > 0 && ROW > 0, "zero-sized array");
    assert_eq!(WROW % ROW, 0, "wrap size is multiple of true size");

    let mut coord_queue = BinaryHeap::new();
    coord_queue.push((Reverse(0), (0, 0)));
    coord_queue.reserve(WCOL * WROW);

    let mut visited = [[false; WCOL]; WROW];
    visited[0][0] = true;

    while let Some((Reverse(risk), coord)) = coord_queue.pop() {
        if coord == (WROW - 1, WCOL - 1) {
            return risk;
        }
        for new_coord in adj4_coords::<WROW, WCOL>(coord) {
            if visited[new_coord.0][new_coord.1] {
                continue;
            }
            visited[new_coord.0][new_coord.1] = true;
            coord_queue.push((Reverse(risk + index_wrapped(grid, new_coord)), new_coord));
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chiton_path_risk_eg() {
        let cave_map = [
            [1, 1, 6, 3, 7, 5, 1, 7, 4, 2],
            [1, 3, 8, 1, 3, 7, 3, 6, 7, 2],
            [2, 1, 3, 6, 5, 1, 1, 3, 2, 8],
            [3, 6, 9, 4, 9, 3, 1, 5, 6, 9],
            [7, 4, 6, 3, 4, 1, 7, 1, 1, 1],
            [1, 3, 1, 9, 1, 2, 8, 1, 3, 7],
            [1, 3, 5, 9, 9, 1, 2, 4, 2, 1],
            [3, 1, 2, 5, 4, 2, 1, 6, 3, 9],
            [1, 2, 9, 3, 1, 3, 8, 5, 2, 1],
            [2, 3, 1, 1, 9, 4, 4, 5, 8, 1],
        ];

        let risk = path_risk::<10, 10, 50, 50>(&cave_map);
        assert_eq!(risk, 315);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();

    let cave_map = read_input::<_, 100, 100>(stdin.lock()).unwrap();
    let risk = path_risk::<100, 100, 500, 500>(&cave_map);
    println!("minimum path risk: {:?}", risk);
}
