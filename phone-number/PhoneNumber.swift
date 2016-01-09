//
//  PhoneNumber.swift
//  exercism
//
//  Created by Eric Bailey on 1/9/16.
//
//

/**
Clean up user-entered phone numbers so they can be sent SMS messages.

The rules are as follows:

- If a phone number is less than ten digits, assume it is a bad number.
- If a phone number is ten digits assume that it is good
- If a phone number is eleven digits and the first number is 1,
  trim the 1 and use the last ten digits
- If a phone number is eleven digits and the first number is not 1, then it is a bad number.
- If the phone number is more than eleven digits, assume it is a bad number.


This exercise was inspired by [Event Manager by JumpstartLab][1].

[1]: http://tutorials.jumpstartlab.com/projects/eventmanager.html
*/
public struct PhoneNumber: CustomStringConvertible {

  /// A string of ten decimal digits.
  public let number: String

  /**
   Given a `startingNumber`, prune non-digits from it and store it as `number`.

   - parameter startingNumber: A string representing a phone number.
   */
  public init(startingNumber: String) {
    let digits = startingNumber.onlyDigits

    switch digits.characters.count {
    case 10:
      number = digits
    case 11 where digits.hasPrefix("1"):
      number = digits.tail
    default:
      number = "0000000000"
    }
  }

  /// The area code of a phone number.
  public var areaCode: String {
    return number[0...2]
  }

  /// The first three digits after the area code in a phone number.
  public var centralOfficeCode: String {
    return number[3...5]
  }

  /// The last four digits of a phone number.
  public var lineNumber: String {
    return number[6...9]
  }

  /// A pretty printed phone number.
  public var description: String {
    return "(\(areaCode)) \(centralOfficeCode)-\(lineNumber)"
  }

}

private extension String {

  /**
   Given a range return a substring.

   If `range.startIndex` is greater than or equal to `endIndex` return `""`.
   If `range.endIndex` is greater than `endIndex`, use `endIndex` instead.

   - parameter range: An integer range.
   - returns: A substring from `range.startIndex` to `range.endIndex` or `endIndex`, or `""`.

   - remark: Modified from xSwift example.
   */
  subscript (range: Range<Int>) -> String {
    get {
      let start = startIndex.advancedBy(range.startIndex)
      guard start < endIndex else { return "" }

      let end   = min(endIndex, start.advancedBy(range.endIndex - range.startIndex))

      return self[Range(start: start, end: end)]
    }
  }

  /// A string comprised of only the characters in `characters` for which `Character.isDigit` holds.
  var onlyDigits: String {
    return String(characters.filter { $0.isDigit })
  }

  var tail: String {
    return substringFromIndex(startIndex.successor())
  }

}

private extension Character {

  /// Return `true` iff `self` is a decimal digit.
  var isDigit: Bool {
    return "0123456789".characters.contains(self)
  }

}
