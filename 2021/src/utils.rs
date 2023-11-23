use std::io::BufRead;
use std::str::FromStr;

pub fn parsing_input<R: BufRead, T: FromStr>(reader: R) -> impl Iterator<Item = T> {
    reader
        .lines()
        .filter_map(|r| r.ok())
        .filter_map(|s| s.parse::<T>().ok())
}

pub struct ArrayWrapper<T, const LEN: usize>(pub [T; LEN]);

impl<T, const LEN: usize> std::iter::FromIterator<T> for ArrayWrapper<T, LEN> {
    fn from_iter<I: IntoIterator<Item = T>>(iterable: I) -> Self {
        let mut result: [T; LEN] = unsafe { std::mem::uninitialized() };

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

        Self(result)
    }
}

impl<T, const LEN: usize> From<ArrayWrapper<T, LEN>> for [T; LEN] {
    fn from(wrapper: ArrayWrapper<T, LEN>) -> Self {
        wrapper.0
    }
}

pub fn n_min<T: core::cmp::Ord, I: Iterator<Item = T>>(n: usize, mut iter: I) -> Vec<T> {
    let mut buffer = std::collections::BinaryHeap::new();

    for item in iter.by_ref().take(n) {
        buffer.push(item);
    }
    for item in iter {
        buffer.push(item);
        buffer.pop();
    }

    buffer.into_sorted_vec()
}
