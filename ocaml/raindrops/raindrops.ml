open Base

let raindrop n =
  let divides d n = Int.rem n d = 0 in
  let raindrops = [ (3, "Pling"); (5, "Plang"); (7, "Plong") ] in
  List.fold_left raindrops ~init:None ~f:(fun acc (d, drop) ->
      if divides d n then
        match acc with None -> Some drop | Some s -> Some (s ^ drop)
      else acc)
  |> Option.value ~default:(Int.to_string n)
