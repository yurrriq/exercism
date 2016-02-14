//
//  Robot.swift
//  exercism
//
//  Created by Eric Bailey on 2/13/16.
//
//

import Darwin

// I'd prefer this as a struct, but then resetName() would
// have to be marked `mutating` and the tests updated accordingly.
public class Robot {

  public var name: String

  public init() {
    name = randomName()
  }

  public func resetName() {
    name = randomName()
  }

}

private func randomName() -> String {
  return "\(randomLetter())\(randomLetter())\(randomNumber())"
}

private func randomLetter() -> UnicodeScalar {
  // 65 => A, 90 => Z
  return UnicodeScalar(randomUInt32(65, 90))
}

private func randomNumber() -> UInt32 {
  return randomUInt32(100, 999)
}

private func randomUInt32(from: UInt32, _ to: UInt32) -> UInt32 {
  return arc4random_uniform(to - from + 1) + from
}
