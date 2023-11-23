from collections import namedtuple, defaultdict
import functools
import operator


def parse_line(line):
    mats, allergens = line.split(' (contains ')
    allergens = allergens.rstrip(')')

    return namedtuple('ReturnTuple', 'mats allergens')(
        mats.split(' '), allergens.split(', ')
    )


def parse_file(text):
    return [parse_line(line) for line in text.split('\n') if line]


def process_allergens(data):
    data = [(a, set(mats)) for (mats, allergens) in data for a in allergens]

    total_mats = set()
    for a, mats in data:
        total_mats |= mats

    grouped_data = {}
    for a, mats in data:
        if a in grouped_data:
            mats &= grouped_data[a]
        grouped_data[a] = mats
            

    confirmed_allergens = {}
    while True:
        for a in grouped_data:
            if a in confirmed_allergens or len(grouped_data[a]) > 1:
                continue
            if len(grouped_data[a]) <= 0:
                raise ValueError

            mat = next(iter(grouped_data[a]))
            confirmed_allergens[a] = mat
            for mats in grouped_data.values():
                mats.discard(mat)
            break
        else:
            break

    for mats in grouped_data.values():
        total_mats -= mats
    for mat in confirmed_allergens.values():
        total_mats.remove(mat)

    return namedtuple('AllergenData', 'confirmed, possible, clean_mats')(
        confirmed=confirmed_allergens,
        possible=grouped_data,
        clean_mats=total_mats,
    )


def count_occurences(data):
    allergen_data = process_allergens(data)
    return sum(
        1 if mat in mats else 0
        for mats, allergens in data
        for mat in allergen_data.clean_mats
    )


def dangerous_list(data):
    allergen_data = process_allergens(data)

    return ','.join(allergen_data.confirmed[a] for a in sorted(allergen_data.confirmed))

