//
//  Bob.swift
//  exercism
//
//  Created by Eric Bailey on 6/30/15.
//
//

import Foundation

class Bob {

  class func hey(input: String) -> String {
    if input.isSilent   { return "Fine, be that way." }
    if input.isYelled   { return "Woah, chill out!"   }
    if input.isQuestion { return "Sure."              }

    return "Whatever."
  }

}


extension String {

  /// Return `true` iff `self` contains only whitespace.
  var isSilent: Bool {
    return stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet()).isEmpty
  }

  /// Return `true` iff `self` has at least one uppercase letter and is equal to itself uppercased.
  var isYelled: Bool {
    let notUppercase = NSCharacterSet.uppercaseLetterCharacterSet().invertedSet
    return !stringByTrimmingCharactersInSet(notUppercase).isEmpty && self == uppercaseString
  }

  /// Return `true` iff `self` ends with `"?"`.
  var isQuestion: Bool {
    return hasSuffix("?")
  }

}
