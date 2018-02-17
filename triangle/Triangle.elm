module Triangle exposing (triangleKind)

{- Desribe the quality of a triangle.

    triangleKind 2 2 2 == Ok "equilateral"
    triangleKind 3 4 5 == Ok "scalene"
    triangleKind 1 1 3 == Err "Violates inequality"
-}
triangleKind : number -> number -> number -> Result String String
triangleKind x y z =
  if x <= 0 || y <= 0 || z <= 0 then
    Err "Invalid lengths"
  else if (x >= (y + z)) || (y >= (z + x)) || (z >= (x + y)) then
    Err "Violates inequality"
  else if x == y && y == z then
    Ok "equilateral"
  else if (x == y) || (x == z) || (y == z) then
    Ok "isosceles"
  else
    Ok "scalene"
