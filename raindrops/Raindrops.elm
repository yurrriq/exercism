module Raindrops exposing (raindrops)

import String exposing (isEmpty)

divides : Int -> Int -> Bool
divides d n = 0 == n `rem` d

raindrops : Int -> String
raindrops n =
  let
    iffDivides : Int -> String -> String
    iffDivides d s = if d `divides` n then s else ""
    pling  = iffDivides 3 "Pling"
    plang  = iffDivides 5 "Plang"
    plong  = iffDivides 7 "Plong"
    output = pling ++ plang ++ plong
  in
    if   isEmpty  output
    then toString n
    else output
