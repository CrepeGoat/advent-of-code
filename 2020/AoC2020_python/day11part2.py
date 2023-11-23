import functools

import numpy as np


def parse_input(input_seq):
    return np.array(
        [
            [{'L': 0, '.': -1}[i] for i in line]
            for line in input_seq[:-1].split('\n')
        ]
    )


def count_visibles(array):
    visibles = np.zeros_like(array)

    # Count along cartesian directions
    line_vis = np.empty_like(array[0])
    for i_range in (range(array.shape[0]), range(array.shape[0])[::-1]):
        line_vis[:] = 0
        for i in i_range:
            visibles[i] += line_vis
            line_vis[array[i] != -1] = array[i, array[i] != -1]

    line_vis = np.empty_like(array[:, 0])
    for i_range in (range(array.shape[1]), range(array.shape[1])[::-1]):
        line_vis[:] = 0
        for i in i_range:
            visibles[:, i] += line_vis
            line_vis[array[:, i] != -1] = array[array[:, i] != -1, i]

    @functools.lru_cache()
    def L_mask(shape, k):
        """
        Generate over a 2D array of the given shape an L-shaped mask
        diagonally offset k spaces from the respective corner of the array.
        """
        if not 0 <= k < min(shape):
            raise ValueError('invalid diagonal corner offset')

        mask = np.zeros_like(array, dtype=bool)
        mask[:(-k or None), k] = True
        mask[-k-1, k:] = True

        return np.nonzero(mask)

    def L_mask_rev(shape, k):
        # just need to invert one axis
        i1, *ix = L_mask(shape, k)
        return (shape[0]-1 - i1,) + tuple(ix)

    # Count along diagonals
    diag_vis = np.empty_like(array[L_mask(array.shape, 0)])
    for i_range in (range(min(array.shape)), range(min(array.shape))[::-1]):
        for make_mask in (L_mask, L_mask_rev):
            diag_vis[:] = 0
            for i in i_range:
                index = slice(i, (-i or None))
                mask = make_mask(array.shape, i)

                visibles[mask] += diag_vis[index]
                diag_vis[index][array[mask] != -1] = array[mask][array[mask] != -1]

    return visibles


def simulate_seats(array):
    mask_floor = (array == -1)
    while True:
        yield array

        array = (4*array >= count_visibles(array)).astype(np.int8)
        array[mask_floor] = -1


def buffered_iter(iterable, length=2):
    iterator = iter(iterable)
    result = (None,) + tuple(item for _, item in zip(range(length-1), iterator))
    for item in iterator:
        result = result[1:] + (item,)
        yield result


def run(input_seq):
    a = parse_input(input_seq)
    for array1, array2 in buffered_iter(simulate_seats(a), length=2):
        if np.all(array1 == array2):
            break

    return np.count_nonzero(array2 == 1)
