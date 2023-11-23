import pytest

from AoC2020_python import day15


@pytest.mark.parametrize('starters, expt_sequence', [
    ([0, 3, 6], [0, 3, 6, 0, 3, 3, 1, 0, 4, 0]),
])
def test_play_the_game(starters, expt_sequence):
    for calc, expt in zip(day15.play_the_game(*starters), expt_sequence):
        assert calc == expt


@pytest.mark.parametrize('starters, expt_2020_result', [
    ([1, 3, 2], 1),
    ([2, 1, 3], 10),
    ([1, 2, 3], 27),
    ([2, 3, 1], 78),
    ([3, 2, 1], 438),
    ([3, 1, 2], 1836),
    ([1, 12, 0, 20, 8, 16], 273)
])
def test_get_2020th(starters, expt_2020_result):
    assert day15.get_2020th(*starters) == expt_2020_result


@pytest.mark.parametrize('starters, expt_30m_result', [
    ([0, 3, 6], 175594),
    ([1, 3, 2], 2578),
    ([2, 1, 3], 3544142),
    ([1, 2, 3], 261214),
    ([2, 3, 1], 6895259),
    ([3, 2, 1], 18),
    ([3, 1, 2], 362),
    ([1, 12, 0, 20, 8, 16], 47205)
])
def test_get_30mth(starters, expt_30m_result):
    assert day15.get_30mth(*starters) == expt_30m_result
