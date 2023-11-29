type nucleotide = A | C | G | T

let hamming_distance l1 l2 =
  let rec go acc xs ys =
    match (xs, ys) with
    | [], [] -> Ok acc
    | x :: xs, y :: ys -> go (acc + Bool.to_int (x <> y)) xs ys
    | _ -> Error "left and right strands must be of equal length"
  in
  match (l1, l2) with
  | [], [] -> Ok 0
  | [], _ -> Error "left strand must not be empty"
  | _, [] -> Error "right strand must not be empty"
  | _ -> go 0 l1 l2
