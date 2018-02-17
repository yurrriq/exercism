//
//  Hamming.swift
//  exercism
//
//  Created by Eric Bailey on 12/28/15.
//
//

import Foundation

class Hamming {

  class func compute(input: String, against: String) -> Int? {
    let encoding      = NSStringEncoding(NSUTF8StringEncoding)
    let lengthInput   = input.lengthOfBytesUsingEncoding(encoding)
    let lengthAgainst = against.lengthOfBytesUsingEncoding(encoding)

    guard lengthInput == lengthAgainst else { return nil }

    return zip(input.characters, against.characters).reduce(0) { $1.0 == $1.1 ? $0 : $0! + 1 }
  }

}
