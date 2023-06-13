from functools import reduce
from operator import mul


def largest_product(series, size):
    if size < 1:
        raise ValueError("span must not be negative")

    if size > len(series):
        raise ValueError("span must be smaller than string length")

    numbers = [digit_to_int(c) for c in list(series)]
    chunks = chunks_of(numbers, size)

    return max(product(chunk) for chunk in chunks)


def product(lst):
    return reduce(mul, lst, 1)


def chunks_of(lst, n):
    for i in range(0, len(lst) - n + 1):
        yield lst[i : i + n]


def digit_to_int(digit):
    if not digit.isdigit():
        raise ValueError("digits input must only contain digits")

    return ord(digit) - 48
