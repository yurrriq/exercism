//
//  Allergies.swift
//  Exercism
//
//  Created by Eric Bailey on 6/6/16.
//
//


public struct Allergies {

  private let score: UInt

  public init(_ score: UInt) {
    self.score = score
  }

  public func hasAllergy(allergen: Allergen) -> Bool {
    return allergen.rawValue & score == allergen.rawValue
  }

  public enum Allergen: UInt {
    case Eggs         = 1
    case Peanuts      = 2
    case Shellfish    = 4
    case Strawberries = 8
    case Tomatoes     = 16
    case Chocolate    = 32
    case Pollen       = 64
    case Cats         = 128
  }

}
