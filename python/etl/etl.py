from functools import reduce


def transform(legacy_data: dict[int, list[str]]) -> dict[str, int]:
    return {
        letter.lower(): score for score in legacy_data for letter in legacy_data[score]
    }
