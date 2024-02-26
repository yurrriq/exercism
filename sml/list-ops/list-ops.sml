fun reverse xs =
  foldl (fn (x, acc) => x :: acc) [] xs

fun filter (pred, xs) =
  foldr (fn (x, ys) => if pred x then x :: ys else ys) [] xs

fun map (f, xs) =
  foldr (fn (x, acc) => f x :: acc) [] xs

fun append (xs, ys) =
  foldr op:: ys xs

fun length xs =
  foldl (fn (_, acc) => acc + 1) 0 xs

fun foldl (_, init, []) = init
  | foldl (f, init, (x :: xs)) =
      foldl (f, (f (init, x)), xs)

fun foldr (_, init, []) = init
  | foldr (f, init, (x :: xs)) =
      f (x, foldr (f, init, xs))

fun concat xxs = foldr (append, [], xxs)
