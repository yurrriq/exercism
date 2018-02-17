open Core.Std

let leap_year =
  let divides d n = n mod d = 0 in
  function year -> divides 400 year ||
                    divides 4 year &&
                      not (divides 100 year)
