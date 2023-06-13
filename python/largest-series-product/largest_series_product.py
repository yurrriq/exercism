from functools import reduce
from operator import mul


def largest_product(series, size):
    if size == 0:
        return 1
    if size < 0:
        raise ValueError("span must not be negative")
    if size > len(series):
        raise ValueError("span must be smaller than string length")
    digits = [digit_to_int(c) for c in list(series)]
    return max(map(product, chunks_of(digits, size)))


def product(lst):
    return reduce(mul, lst, 1)


def chunks_of(lst, n):
    for i in range(0, len(lst) - n + 1):
        yield lst[i : i + n]


def digit_to_int(digit):
    if not digit.isdigit():
        raise ValueError("digits input must only contain digits")
    return int(digit)
