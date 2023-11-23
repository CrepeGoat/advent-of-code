use aoc_lib::grid::{adj8_coords, read_input, Grid};

use itertools::Itertools;

fn advance_epoch<const ROW: usize, const COL: usize>(
    mut mat: Grid<u32, ROW, COL>,
) -> Grid<u32, ROW, COL> {
    const FLASH_THRESHOLD: u32 = 10;
    for (i, j) in (0..ROW).flat_map(|i| (0..COL).map(move |j| (i, j))) {
        mat[i][j] += 1;
    }

    let mut flash_stack: Vec<_> = (0..ROW)
        .flat_map(|i| (0..COL).map(move |j| (i, j)))
        .filter_map(|(i, j)| (mat[i][j] == FLASH_THRESHOLD).then(|| (i, j)))
        .collect();
    while let Some(coord) = flash_stack.pop() {
        for (i, j) in adj8_coords::<ROW, COL>(coord) {
            if mat[i][j] < FLASH_THRESHOLD {
                mat[i][j] += 1;
                if mat[i][j] == FLASH_THRESHOLD {
                    flash_stack.push((i, j));
                }
            }
        }
    }

    for (i, j) in (0..ROW).flat_map(|i| (0..COL).map(move |j| (i, j))) {
        if mat[i][j] == FLASH_THRESHOLD {
            mat[i][j] = 0;
        }
    }

    mat
}

fn sync_epoch<const ROW: usize, const COL: usize>(mut mat: Grid<u32, ROW, COL>) -> u32 {
    fn is_synced<const ROW: usize, const COL: usize>(mat: &Grid<u32, ROW, COL>) -> bool {
        (0..ROW)
            .cartesian_product(0..COL)
            .all(|(i, j)| mat[i][j] == 0)
    }

    let mut count: u32 = 0;
    while !is_synced(&mat) {
        count += 1;
        mat = advance_epoch(mat);
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_advance_epoch_small_eg1() {
        let dumbo_grid = [
            [1, 1, 1, 1, 1],
            [1, 9, 9, 9, 1],
            [1, 9, 1, 9, 1],
            [1, 9, 9, 9, 1],
            [1, 1, 1, 1, 1],
        ];
        let dumbo_grid_next = advance_epoch(dumbo_grid);
        assert_eq!(
            dumbo_grid_next,
            [
                [3, 4, 5, 4, 3],
                [4, 0, 0, 0, 4],
                [5, 0, 0, 0, 5],
                [4, 0, 0, 0, 4],
                [3, 4, 5, 4, 3],
            ],
        );
    }

    #[test]
    fn test_advance_epoch_small_eg2() {
        let dumbo_grid = [
            [3, 4, 5, 4, 3],
            [4, 0, 0, 0, 4],
            [5, 0, 0, 0, 5],
            [4, 0, 0, 0, 4],
            [3, 4, 5, 4, 3],
        ];
        let dumbo_grid_next = advance_epoch(dumbo_grid);
        assert_eq!(
            dumbo_grid_next,
            [
                [4, 5, 6, 5, 4],
                [5, 1, 1, 1, 5],
                [6, 1, 1, 1, 6],
                [5, 1, 1, 1, 5],
                [4, 5, 6, 5, 4],
            ],
        );
    }

    #[test]
    fn test_sync_epoch_eg() {
        let dumbo_grid = [
            [5, 4, 8, 3, 1, 4, 3, 2, 2, 3],
            [2, 7, 4, 5, 8, 5, 4, 7, 1, 1],
            [5, 2, 6, 4, 5, 5, 6, 1, 7, 3],
            [6, 1, 4, 1, 3, 3, 6, 1, 4, 6],
            [6, 3, 5, 7, 3, 8, 5, 4, 7, 8],
            [4, 1, 6, 7, 5, 2, 4, 6, 4, 5],
            [2, 1, 7, 6, 8, 4, 1, 7, 2, 1],
            [6, 8, 8, 2, 8, 8, 1, 1, 3, 4],
            [4, 8, 4, 6, 8, 4, 8, 5, 5, 4],
            [5, 2, 8, 3, 7, 5, 1, 5, 2, 6],
        ];
        assert_eq!(sync_epoch(dumbo_grid), 195);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();

    let dumbo_grid = read_input::<_, 10, 10>(stdin.lock()).unwrap();
    println!("epoch 0: {:?}", dumbo_grid);

    println!("first sync: {:?}", sync_epoch(dumbo_grid));
}
