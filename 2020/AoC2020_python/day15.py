import itertools


def play_the_game(*starters):
    yield from starters

    recents = {
        s: i
        for i, s in enumerate(starters[:-1], 1)
    }

    last_item = starters[-1]
    for counter in itertools.count(len(starters)):
        if last_item in recents:
            next_item = counter - recents[last_item]
        else:
            next_item = 0
        yield next_item
        recents[last_item] = counter
        last_item = next_item


def get_2020th(*starters):
    return next(
        value
        for (i, value) in enumerate(play_the_game(*starters), 1)
        if i == 2020
    )


def get_30mth(*starters):
    return next(
        value
        for (i, value) in enumerate(play_the_game(*starters), 1)
        if i == 30 * 10**6
    )
