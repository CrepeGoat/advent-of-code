import functools
import operator
import numpy as np

input_seq = [
    49, 89, 70, 56, 34, 14, 102, 148, 143, 71, 15, 107, 127, 165, 135, 26, 119,
    46, 53, 69, 134, 1, 40, 81, 140, 160, 33, 117, 82, 55, 25, 11, 128, 159, 61,
    105, 112, 99, 93, 151, 20, 108, 168, 2, 109, 75, 139, 170, 65, 114, 21, 92,
    106, 162, 124, 158, 38, 136, 95, 161, 146, 129, 154, 121, 86, 118, 88, 50,
    48, 62, 155, 28, 120, 78, 60, 147, 87, 27, 7, 54, 39, 113, 5, 74, 169, 6,
    43, 8, 29, 18, 68, 32, 19, 133, 22, 94, 47, 132, 59, 83, 12, 13, 96, 35,
]

a = np.sort(input_seq)
a_diffs = np.diff(a, prepend=0, append=a[-1]+3)
assert np.all(np.unique(a_diffs) == [1, 3])  # VERY important

_1s_streak_bounds = np.flatnonzero(np.diff(a_diffs, prepend=3))
_1s_streaks = np.diff(_1s_streak_bounds)[::2]


@functools.lru_cache()
def tribonacci(n):
    """
    Calculate the nth "tribonacci" number, satisfying the equation:
        a_-1 = a_0 = a_1 = 1
        a_n = a_(n-1) + a_(n-2) + a_(n-3)
    """
    if n <= 0:
        return 0
    if n == 1:
        return 1
    return tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)


valid_combos = functools.reduce(
    operator.mul,
    (tribonacci(i) for i in _1s_streaks+1),
    1,
)
print(valid_combos)
