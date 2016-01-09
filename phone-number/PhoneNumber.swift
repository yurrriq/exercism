//
//  PhoneNumber.swift
//  exercism
//
//  Created by Eric Bailey on 1/9/16.
//
//

import Foundation

public struct PhoneNumber: CustomStringConvertible {

  public var startingNumber: String

  public init(startingNumber: String) {
    self.startingNumber = startingNumber
  }

  public func number() -> String {
    let digits = String(startingNumber.characters.filter { $0.isDigit })

    switch digits.characters.count {
    case 10:
      return digits
    case 11 where digits.characters.first == "1":
      return digits.tail
    default:
      return "0000000000"
    }
  }


  public func areaCode() -> String {
    return number().substringToIndex(startingNumber.startIndex.advancedBy(3))
  }


  // TODO: Make PR to update test: s/description\(\)/description/g
  public var description: String {
    let digits            = number()
    let areaCode          = digits.substringToIndex(digits.startIndex.advancedBy(3))
    let centralOfficeCode = digits.substringWithRange(Range(
      start: digits.startIndex.advancedBy(3),
      end:   digits.startIndex.advancedBy(6)))
    let lineNumber        = digits.substringFromIndex(digits.startIndex.advancedBy(6))

    return "(\(areaCode)) \(centralOfficeCode)-\(lineNumber)"
  }

}

public extension String {

  public var tail: String {
    return substringFromIndex(startIndex.successor())
  }

}

public extension Character {

  public var isDigit: Bool {
    return "0123456789".characters.contains(self)
  }

}
