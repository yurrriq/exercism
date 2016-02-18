//
//  Robot.swift
//  exercism
//
//  Created by Eric Bailey on 2/13/16.
//
//

import Darwin


/**
 Manage robot factory settings.

 When robots come off the factory floor, they have no name.

 The first time you boot them up, a random name is generated, such as
 RX837 or BC811.

 Every once in a while we need to reset a robot to its factory settings,
 which means that their name gets wiped. The next time you ask, it will
 respond with a new random name.

 The names must be random: they should not follow a predictable sequence.
 Random names means a risk of collisions. Your solution should not allow
 the use of the same name twice when avoidable. In some exercism language
 tracks there are tests to ensure that the same name is never used twice.


 This exercise comes from a debugging session with Paul Blackwell at [gSchool](http://gschool.it).

 - remark: I'd prefer this as a `struct`, but then `Robot.resetName()` would have to be
 marked `mutating` and `RobotNameTest` updated accordingly.
 */
public class Robot {

  /// A `Robot`'s `name` is a `String`.
  public var name: String

  /**
   Initialize a new `Robot` with a random `name`.
   - seealso: `randomName()`
   */
  public init() {
    name = randomName()
  }

  /**
   Reset `self`'s `name`, i.e. set it to a new random name.
   - seealso: `resetName()`
   */
  public func resetName() {
    name = randomName()
  }

}

/**
 Generate a random name, composed of two random letters followed by a random three-digit number.

 - returns: A random `Robot` `name`.
 - seealso: `randomLetter()`, `randomNumber()`
 */
private func randomName() -> String {
  return "\(randomLetter())\(randomLetter())\(randomNumber())"
}

/**
 Generate a random capital letter.

 - returns: A random capital letter.
 */
private func randomLetter() -> UnicodeScalar {
  // 65 => A, 90 => Z
  return UnicodeScalar(randomUInt32(65, 90))
}


/**
 Generate a random three-digit number.

 - returns: A random three-digit number.
 - seealso: `randomUInt32(_:_:)`
 */
private func randomNumber() -> UInt32 {
  return randomUInt32(100, 999)
}

/**
 Generate a random number between `from` and `to`, inclusive.

 - parameter from: An inclusive lower bound.
 - parameter to: An inclusive upper bound.
 - returns: A random number between `from` and `to`, inclusive.
 */
private func randomUInt32(from: UInt32, _ to: UInt32) -> UInt32 {
  return arc4random_uniform(to - from + 1) + from
}
