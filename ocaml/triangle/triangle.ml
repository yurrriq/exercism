let is_triangle x y z =
  x > 0 && y > 0 && z > 0 && x + y > z && x + z > y && y + z > x

let is_equilateral x y z = is_triangle x y z && x = y && y = z
let is_isosceles x y z = is_triangle x y z && (x = y || x = z || y = z)
let is_scalene x y z = is_triangle x y z && x <> y && x <> z && y <> z
