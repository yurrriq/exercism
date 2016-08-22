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

module LeapYear

let isLeapYear =
  let divides d n = n % d = 0
  function
  | year when divides 400 year -> true
  | year when divides 100 year -> false
  | year when divides   4 year -> true
  | _                          -> false
