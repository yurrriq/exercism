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

module SpaceAge

open System

type Planet = Earth
            | Jupiter
            | Mars
            | Mercury
            | Neptune
            | Saturn
            | Uranus
            | Venus

let orbitalPeriod = (*) 31557600.0m << function
  | Earth   ->   1.0m
  | Jupiter ->  11.862615m
  | Mars    ->   1.8808158m
  | Mercury ->   0.2408467m
  | Neptune -> 164.79132m
  | Saturn  ->  29.447498m
  | Uranus  ->  84.016846m
  | Venus   ->   0.61519726m

let flip f x y = f y x

let divToHundreths (n : decimal) (d : decimal) = Math.Round(n / d, 2)

let spaceAge = flip divToHundreths << orbitalPeriod
