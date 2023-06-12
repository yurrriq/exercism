from functools import reduce


def transform(legacy_data):
    return reduce(
        lambda new_data, score: new_data
        | dict.fromkeys([letter.lower() for letter in legacy_data[score]], score),
        legacy_data,
        {},
    )
