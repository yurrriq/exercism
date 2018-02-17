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

module DifferenceOfSquares

/// Return the square of the sum of the first `n` natural numbers.
let squareOfSums (n : int) : int =
  let square m = m * m
  (n * (n + 1)) / 2 |> square

/// Return the sum of the squares of the first `n` natural numbers.
let sumOfSquares (n : int) : int = (n * (n + 1) * (2 * n + 1)) / 6

/// Return the difference between the sum of the squares
/// and the square of the sum of the first `n` natural numbers.
let difference (n : int) : int = squareOfSums n - sumOfSquares n
