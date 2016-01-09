//
//  ETL.swift
//  exercism
//
//  Created by Eric Bailey on 12/30/15.
//
//

/// A score is an integer.
public typealias Score    = Int

/// A letter is a one-character-long string.
public typealias Letter   = String

/// Old data are dictionaries from a score to an array of letters.
public typealias OldData  = [Score: [Letter]]

/// An old datum is a pair of a score and an array of letters.
public typealias OldDatum = (Score, [Letter])

/// New data are dictionaries from letter to score.
public typealias NewData  = [Letter: Score]

/// A new datum is a pair of a letter and a score.
public typealias NewDatum = (Letter, Score)

/**
 Doing the `Transform` step of an Extract-Transform-Load,
 i.e. extract some Scrabble scores from a legacy system.


 Extract-Transform-Load (ETL) is a fancy way of saying,
 > We have some crufty, legacy data over in this system,
 > and now we need it in this shiny new system over here,
 > so we're going to migrate this."

 Typically, this is followed by,
 > We're only going to need to run this once.


 The old system stored a list of letters per score:

 - 1 point: "A", "E", "I", "O", "U", "L", "N", "R", "S", "T",
 - 2 points: "D", "G",
 - 3 points: "B", "C", "M", "P",
 - 4 points: "F", "H", "V", "W", "Y",
 - 5 points: "K",
 - 8 points: "J", "X",
 - 10 points: "Q", "Z",

 In the shiny new scrabble system, store the score per letter (`NewData`) instead,
 which makes for faster and easier calculations of scores for words.
 Also, store the letters in lowercase, regardless of the case of the input letters:

 - "a" is worth 1 point.
 - "b" is worth 3 points.
 - "c" is worth 3 points.
 - "d" is worth 2 points.

 This exercise comes from the [Jumpstart Lab](http://jumpstartlab.com) team.
 */
public class ETL {

  /**
   Given some `OldData`, update it to the new system and return `NewData`.

   That is, convert each `Letter` in `oldData` to lowercase and
   return a dictionary from `Letter` to `Score`.

   - parameter oldData: Some `OldData`.
   - returns: `NewData`.
   */
  public class func transform(oldData: OldData) -> NewData {
    return oldData
      .flatMap { (score, letters) -> [NewDatum] in
        letters.map { letter in (letter.lowercaseString, score) }
      }
      .reduce([:]) { (var acc, elem) in
        acc[elem.0] = elem.1
        return acc
    }
  }

}
