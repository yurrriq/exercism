hypot(.x; .y) as $distance |
[[10, 0], [5, 1], [1, 5]] |
map(select($distance > first) | last) |
first // 10
