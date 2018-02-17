let rec fold ~init:acc ~f = function
  | []        -> acc
  | (x :: xs) -> fold ~init:(f acc x) ~f xs

let length l =
  let rec go acc = function
    | [] -> acc
    | _ :: t -> go (acc+1) t in
  go 0 l

(* let length = fold ~init:0 ~f:(fun acc x -> acc + 1) *)

let reverse xs = fold ~init:[] ~f:(fun acc x -> x :: acc) xs

let map ~f xs =
  fold ~init:[] ~f:(fun acc x -> f x :: acc) xs
  |> reverse

let append xs ys =
  fold ~init:ys ~f:(fun acc x -> x :: acc) xs
  |> reverse

let concat xs =
  fold ~init:[] ~f:append xs |> reverse

let filter ~f xs =
  let g acc = function
    | x when f x -> x :: acc
    | _          -> acc in
  fold ~init:[] ~f:g xs |> reverse
