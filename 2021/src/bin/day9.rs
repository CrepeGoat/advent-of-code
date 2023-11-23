use aoc_lib::grid::{adj4_coords, read_input, Grid};
use aoc_lib::utils::n_min;

fn find_low_points<const ROW: usize, const COL: usize>(
    cave_map: &Grid<u32, ROW, COL>,
) -> Grid<bool, ROW, COL> {
    use std::cmp::Ordering::*;

    let mut result = [[true; COL]; ROW];

    for (res_row, cave_row) in (&mut result[..]).iter_mut().zip(&cave_map[..]) {
        for j in 0..(COL - 1) {
            match cave_row[j].cmp(&cave_row[j + 1]) {
                Less => res_row[j + 1] = false,
                Greater => res_row[j] = false,
                _ => {
                    res_row[j] = false;
                    res_row[j + 1] = false;
                }
            }
        }
    }

    for j in 0..COL {
        for i in 0..(ROW - 1) {
            match cave_map[i][j].cmp(&cave_map[i + 1][j]) {
                Less => result[i + 1][j] = false,
                Greater => result[i][j] = false,
                _ => (),
            }
        }
    }

    result
}

fn risk_levels<'a, const ROW: usize, const COL: usize>(
    cave_map: &'a Grid<u32, ROW, COL>,
    low_points: &'a Grid<bool, ROW, COL>,
) -> impl 'a + Iterator<Item = u32> {
    (0..ROW)
        .flat_map(|i| (0..COL).map(move |j| (i, j)))
        .filter_map(move |(i, j)| low_points[i][j].then(|| 1 + cave_map[i][j]))
}

fn iter_basins<const ROW: usize, const COL: usize>(
    cave_map: &Grid<u32, ROW, COL>,
    mut low_points: Grid<bool, ROW, COL>,
) -> impl '_ + Iterator<Item = usize> {
    let iter_low_points = (0..ROW)
        .flat_map(|i| (0..COL).map(move |j| (i, j)))
        .filter(|(i, j)| low_points[*i][*j])
        .collect::<Vec<_>>();

    let mut point_buffer = Vec::new();
    iter_low_points.into_iter().map(move |low_point| {
        let mut count = 0;
        point_buffer.push(low_point);

        while let Some(coord) = point_buffer.pop() {
            count += 1;

            for (i, j) in adj4_coords::<ROW, COL>(coord) {
                if low_points[i][j]
                    || cave_map[i][j] >= 9
                    || cave_map[i][j] < cave_map[coord.0][coord.1]
                {
                    continue;
                }
                point_buffer.push((i, j));
                low_points[i][j] = true;
            }
        }

        count
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_low_points_example() {
        let cave_map = [
            [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
            [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
            [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
            [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
            [9, 8, 9, 9, 9, 6, 5, 6, 7, 8],
        ];
        let low_points = find_low_points(&cave_map);
        assert_eq!(
            low_points,
            [
                [false, true, false, false, false, false, false, false, false, true],
                [false, false, false, false, false, false, false, false, false, false],
                [false, false, true, false, false, false, false, false, false, false],
                [false, false, false, false, false, false, false, false, false, false],
                [false, false, false, false, false, false, true, false, false, false],
            ]
        );
    }

    #[test]
    fn test_risk_levels_example() {
        let cave_map = [
            [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
            [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
            [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
            [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
            [9, 8, 9, 9, 9, 6, 5, 6, 7, 8],
        ];
        let low_points = find_low_points(&cave_map);
        assert_eq!(
            risk_levels(&cave_map, &low_points).collect::<Vec<_>>(),
            vec![2, 1, 6, 6]
        );
    }

    #[test]
    fn test_iter_basins_example() {
        let cave_map = [
            [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
            [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
            [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
            [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
            [9, 8, 9, 9, 9, 6, 5, 6, 7, 8],
        ];
        let low_points = find_low_points(&cave_map);
        assert_eq!(
            iter_basins(&cave_map, low_points).collect::<Vec<_>>(),
            vec![3, 9, 14, 9],
        );
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();

    let cave_map = read_input::<_, 100, 100>(stdin.lock()).unwrap();
    let low_points = find_low_points(&cave_map);
    let basins: Vec<_> = n_min(3, iter_basins(&cave_map, low_points).map(std::cmp::Reverse))
        .into_iter()
        .map(|r| r.0)
        .collect();
    println!("basins: {:?}", basins);
    println!("basin product: {:?}", basins.into_iter().product::<usize>());
}
