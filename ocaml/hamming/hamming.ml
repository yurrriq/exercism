type nucleotide = A | C | G | T

let hamming_distance xs ys =
  match (xs, ys) with
  | [], [] -> Ok 0
  | [], _ -> Error "left strand must not be empty"
  | _, [] -> Error "right strand must not be empty"
  | _ -> (
      try
        List.fold_left2 (fun acc x y -> acc + Bool.to_int (x <> y)) 0 xs ys
        |> Result.ok
      with _ -> Error "left and right strands must be of equal length")
