def sum_of_multiples(limit, values):
    return sum(
        {
            multiple
            for base in values
            if base != 0
            for multiple in range(base, limit, base)
        }
    )
