type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let rec map_option f = function
  | [] -> []
  | x :: xs -> (
      let rs = map_option f xs in
      match f x with None -> rs | Some r -> r :: rs)

let to_rna =
  let transcribe = function
    | `G -> Some `C
    | `C -> Some `G
    | `T -> Some `A
    | `A -> Some `U
    | _ -> None
  in
  map_option transcribe
