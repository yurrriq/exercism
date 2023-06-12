def sum_of_multiples(limit, values):
    return sum(
        set(
            [
                multiple
                for multiples in [
                    range(base, limit, base) for base in values if base != 0
                ]
                for multiple in multiples
            ]
        )
    )
