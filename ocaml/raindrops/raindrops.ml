open Base


let raindrop n =
  let divides d n = Int.rem n d = 0 in
  let raindrops = [(3, "Pling"); (5, "Plang"); (7, "Plong")] in
  Option.value ~default:(Int.to_string n)
    (List.fold_left raindrops
       ~init:None
       ~f:(fun acc (d, drop) -> if divides d n then
                                  (* FIXME: Option.map acc ~f:((^) drop) *)
                                  match acc with
                                  | None   -> Some drop
                                  | Some s -> Some (s ^ drop)
                                else
                                  acc))
