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

module SumOfMultiples

/// Return the sum of multiples of `d` in `ds` up to, but not including, `n`.
let sumOfMultiples ds n =
  let divides    n  = fun d -> n % d = 0
  let anyDivides ds = List.filter (fun n -> List.exists (divides n) ds)
  List.sum <| anyDivides ds [1..n - 1]
