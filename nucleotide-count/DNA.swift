//
//  DNA.swift
//  exercism
//
//  Created by Eric Bailey on 1/9/16.
//
//

import Foundation

/*

The tests are written s.t. I'm precluded from doing this...

// TODO: Constrain to ACGT
/// A naïve `Nucleotide` is a `Character`.
public typealias Nucleotide = Character
*/

/// A `DNA` `Strand` is a `String`.
public typealias Strand = String

public struct DNA {

  public let strand: Strand

  public init?(strand: Strand) {
    guard strand.nucleotides.reduce(true, combine: { $0 && $1.isValidNucleotide }) else {
      return nil
    }

    self.strand = strand
  }

  public func count(nucleotide: Character) -> Int {
    return strand.nucleotides.reduce(0) { $0 + ($1 == nucleotide ? 1 : 0) }
  }

  public func counts() -> [String: Count] {
    return strand.nucleotides.reduce(["A": 0, "C": 0, "G": 0, "T": 0]) { (var acc, nucleotide) in
      // TODO: Amend the tests and make a PR
      // This string nonsense is to appease the current tests.
      acc["\(nucleotide)"] = (acc["\(nucleotide)"] ?? 0).successor()
      return acc
    }
  }

}

extension Strand {

  var nucleotides: CharacterView {
    return characters
  }

}

extension Character {

  var isValidNucleotide: Bool {
    return ["A", "C", "G", "T"].contains(self)
  }

}
