import numpy as np

x = None

'''
t = 939
line_ids = [7, 13, x, x, 59, x, 31, 19]
'''
t = 1003681
line_ids = [
    23, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 37, x, x, x, x, x, 431,
    x, x, x, x, x, x, x, x, x, x, x, x, 13, 17, x, x, x, x, 19, x, x, x, x, x,
    x, x, x, x, x, x, 409, x, x, x, x, x, x, x, x, x, 41, x, x, x, x, x, x, x,
    x, x, x, x, x, x, x, x, x, x, x, 29,
]
#'''
mod_rem_pairs = [
    (id_, -delay % id_)
    for delay, id_ in enumerate(line_ids)
    if id_ is not None
]


def solve_linear_congruence(a, b, m):
    """Solve the congruence ax === b mod m."""
    a, b = a % m, b % m

    for x in range(m):
        if (a*x) % m == b:
            return x
    else:
        raise RuntimeError


def solve_modulo_system(iterable):
    total_mod = 1
    x = 0

    for mod, rem in iterable:
        x += total_mod * solve_linear_congruence(total_mod, rem-x, mod)
        total_mod *= mod
        x %= total_mod

    return x


print(solve_modulo_system(mod_rem_pairs))
