def saddle_points(matrix):
    if matrix == []:
        return []

    width = len(matrix[0])
    if any(len(row) != width for row in matrix):
        raise ValueError("irregular matrix")

    height = len(matrix)
    row_maxes = [max(row) for row in matrix]
    col_mins = [min(matrix[y][x] for y in range(height)) for x in range(width)]

    return [
        {"column": x + 1, "row": y + 1}
        for (y, row_max) in enumerate(row_maxes)
        for (x, col_min) in enumerate(col_mins)
        if row_max == col_min
    ]
