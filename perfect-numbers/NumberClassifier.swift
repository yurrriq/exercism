//
//  NumberClassifier.swift
//  Exercism
//
//  Created by Eric Bailey on 6/6/16.
//
//

import Foundation

public struct NumberClassifier {

  /** A number to classify.
   - seealso: `classification`
   */
  public var number: Int

  /**
   The Aliquot sum of `number`, i.e. the sum of its factors, excluding itself.
   */
  private var aliquotSum: Int {
    return [Int](1..<number).reduce(0) { $1.divides(number) ? $0 + $1 : $0 }
  }

  /// The Nichomacus `Classification` of `number`.
  public var classification: Classification {
    switch NSNumber(long: aliquotSum).compare(NSNumber(long: number)) {
    case .OrderedSame:       return .Perfect
    case .OrderedDescending: return .Abundant
    case .OrderedAscending:  return .Deficient
    }
  }

  /** Initialize a new `NumberClassifier`.
   - parameter number: An `Int` representing a natural number to be classified.
   */
  public init(number: Int) {
    self.number = number
  }

  /// A natural number classification, as devised by Nicomachus.
  public enum Classification: Int, CustomStringConvertible {

    /// Sum of factors = number
    case Perfect = 0

    /// Sum of factors > number
    case Abundant

    /// Sum of factors < number
    case Deficient

    /// A textual representation of `self`.
    public var description: String {
      switch self {
      case .Perfect:   return "Sum of factors = number"
      case .Abundant:  return "Sum of factors > number"
      case .Deficient: return "Sum of factors < number"
      }
    }
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
