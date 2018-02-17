module RunLengthEncoding exposing (decode, encode, version)

import Char
import Exts.Maybe exposing (maybe)
import List       exposing (head, length, map)
import List.Extra exposing (group, span, splitAt)
-- import Mom        exposing (ap)
import String     exposing (append)

decode : String -> String
decode = decode' [] << String.toList

decode' : List String -> List Char -> String
decode' acc chars =
  if   List.isEmpty chars
  then String.reverse (String.concat acc)
  -- TODO: Figure out if there are arrows in Elm
  else let (digits,chars') = span Char.isDigit chars
           (c,rest)        = splitAt 1 chars'
           s               = String.fromList c
       in  flip decode' rest <|
             case (fromListToInt digits) of
               Ok  n -> String.repeat n s :: acc
               Err _ ->                 s :: acc

encode : String -> String
encode = String.concat << map encode' << group << String.toList

encode' : List Char -> String
encode' chars = maybe "" (oneOrMore (length chars)) (head chars)
-- FIXME: Perhaps List isn't a Mom.
-- encode' = ap (maybe "" << oneOrMore << length) head

oneOrMore : Int -> Char -> String
oneOrMore n char = String.fromChar char
                |> if 1 == n then identity else append (toString n)

version : Int
version = 2

charToInt : Char -> Int
charToInt c = Char.toCode c - Char.toCode '0'

fromListToInt : List Char -> Result String Int
fromListToInt = String.fromList >> String.toInt
