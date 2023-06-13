def saddle_points(matrix):
    if matrix == []:
        return []

    width = len(matrix[0])
    if any(len(row) != width for row in matrix):
        raise ValueError("irregular matrix")

    height = len(matrix)
    row_maxes = {
        (x, y)
        for (y, row) in enumerate(matrix)
        for (x, val) in enumerate(row)
        if matrix[y][x] == max(row)
    }
    col_mins = {
        (x, y)
        for y in range(height)
        for x in range(width)
        if matrix[y][x] == (col_min := min(matrix[y][x] for y in range(height)))
    }

    return [{"column": x + 1, "row": y + 1} for (x, y) in (row_maxes & col_mins)]
