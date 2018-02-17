//
//  WordCount.swift
//  exercism
//
//  Created by Eric Bailey on 12/28/15.
//
//

import Foundation

/// A `Word` is a `String`.
public typealias Word = String

/// A `Count` is an `Int`.
public typealias Count = Int


public struct WordCount {

  let words: String

  init(words: String) {
    self.words = words
  }

  public func count() -> [Word: Count] {
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
