open Core.Std

let response_for =
  let is_silent    = String.for_all ~f:Char.is_whitespace in
  let is_yelled s  = String.exists ~f:Char.is_uppercase s &&
                       String.for_all ~f:(fun c -> Char.is_uppercase c ||
                                                  not (Char.is_alpha c)) s in
  let is_question  = String.is_suffix ~suffix:"?" in
  function
  | s when is_silent   s -> "Fine. Be that way!"
  | s when is_yelled   s -> "Whoa, chill out!"
  | s when is_question s -> "Sure."
  | _                    -> "Whatever."
