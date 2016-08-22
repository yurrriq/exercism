//             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                     Version 2, December 2004
//
//  Copyright (C) 2016 Eric Bailey <eric@ericb.me>
//
//  Everyone is permitted to copy and distribute verbatim or modified
//  copies of this license document, and changing it is allowed as long
//  as the name is changed.
//
//             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//   0. You just DO WHAT THE FUCK YOU WANT TO.
//

module Bob

open System

let hey =
  let isSilent = String.forall Char.IsWhiteSpace
  let isUpperOrNonAlpha (c : char) = Char.IsUpper c || not (Char.IsLetter c)
  let isYelled (s : string) = String.exists Char.IsUpper s
                                && String.forall isUpperOrNonAlpha s
  let isQuestion (s : string) = s.EndsWith("?")
  function
  | s when isSilent   s -> "Fine. Be that way!"
  | s when isYelled   s -> "Whoa, chill out!"
  | s when isQuestion s -> "Sure."
  | _                   -> "Whatever."
