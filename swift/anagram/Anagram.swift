//
//  Anagram.swift
//  exercism
//
//  Created by Eric Bailey on 1/8/16.
//
//

import Foundation


/**
 Given a word and an array of possible anagrams, return the correct subarray.

 For example, given `"listen"` and an array of candidates,

 ```swift
 ["enlists", "google", "inlets", "banana"]
 ```

 ... return the singleton array,

 ```swift
 ["inlets"]
 ```

 This exercise was inspired by the [Extreme Startup game][1].

 [1]: https://github.com/rchatley/extreme_startup
 */
public struct Anagram {

  /// A `Word` for which to find anagrams.
  public let word: Word

  /**
   Create an `Anagram` with a given `word`.

   - parameter word: A `Word` for which to find anagrams.
   */
  public init(word: Word) {
    self.word = word
  }

  /**
   Given an array of `candidates`, return the possibly empty array
   of its elements that are anagrams of `word`.

   - parameter candidates: An array of potential anagrams of `word`.
   - returns: The subarray of `candidates` which are anagrams of `word`.
   */
  public func match(candidates: [Word]) -> [Word] {
    return candidates.filter { $0.lowercaseString.isAnagram(word.lowercaseString) }
  }

}

public extension Word {

  /**
   Given a lowercased word, return `true` iff `self`
   is not equal to and has the same letters as `word`.

   - parameter word: A `Word` of which `self` might be an anagram.
   - returns: `true` iff `self` is an anagram of `word`.

   - requires: `self` and `word` must be lowercased.
   - seealso: `hasSameLetters(_:)`
   */
  public func isAnagram(word: Word) -> Bool {
    return self != word && self.hasSameLetters(word)
  }

  /**
   Given a lowercased word, return `true` iff `self` has the same letters as `word`.

   - parameter word: A word with which to compare `self`.
   - returns: `true` iff `self` has the same letters as `word`.

   - requires: `self` and `word` must be lowercased.
   */
  public func hasSameLetters(word: Word) -> Bool {
    return characters.sort() == word.characters.sort()
  }

}
