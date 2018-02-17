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

module RNATranscription

/// Given a DNA strand, return its RNA complement, per RNA transcription.
let toRna =
  let transcribe = function
    | 'G' -> 'C'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'A' -> 'U'
    |  n  -> invalidOp <| sprintf "'%c' is not a valid nucleotide" n
  String.map transcribe
