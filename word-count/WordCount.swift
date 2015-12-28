//
//  WordCount.swift
//  exercism
//
//  Created by Mo Hacker on 12/28/15.
//
//

import Foundation

/// A `Word` is a `String`.
typealias Word = String

/// A `Count` is an `Int`.
typealias Count = Int


struct WordCount {

  let words: String

  init(words: String) {
    self.words = words
  }

  func count() -> [Word: Count] {
    let nonAlphanumeric = NSCharacterSet.alphanumericCharacterSet().invertedSet
    let substrings      = words.componentsSeparatedByCharactersInSet(nonAlphanumeric)

    return substrings.reduce([Word: Count]()) { (var dict, string) in
      guard !string.isEmpty else { return dict }

      let word = string.lowercaseString
      dict.updateValue(1 + (dict[word] ?? 0), forKey: word)

      return dict
    }
  }

}
