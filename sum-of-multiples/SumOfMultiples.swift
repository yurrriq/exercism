//
//  SumOfMultiples.swift
//  Exercism
//
//  Created by Eric Bailey on 6/17/16.
//
//

public struct SumOfMultiples {

  public static func toLimit(number: Int, inMultiples multiples: [Int]) -> Int {
    return Array(1..<number).anyDivides(multiples.filter { $0 != 0 }).sum()
  }

}

private extension Int {

  /** Return `true` iff `self` divides `dividend`.
   - parameter dividend: A number to divide by `self`
   - returns: `true` if `self` divides `dividend`, otherwise `false`.
   */
  func divides(dividend: Int) -> Bool {
    return dividend % self == 0
  }

}

private extension CollectionType where Generator.Element == Int {

  func anyDivides(factors: [Int]) -> [Int] {
    return filter { number in
      for factor in factors {
        if factor.divides(number) { return true }
      }

      return false
    }
  }

  func sum() -> Int {
    return reduce(0, combine: +)
  }

}
