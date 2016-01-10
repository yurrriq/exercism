//
//  GradeSchool.swift
//  exercism
//
//  Created by Eric Bailey on 1/9/16.
//
//

/**
Store students' names along with the grade they are in.

Usage:

- Add a student's name to the roster for a grade
  - "Add Jim to grade 2."
  - "OK."
- Get a list of all students enrolled in a grade
  - "Which students are in grade 2?"
  - "We've only got Jim just now."
- Get a sorted list of all students in all grades.  Grades should sort
  as 1, 2, 3, etc., and students within a grade should be sorted
  alphabetically by name.
  - "Who all is enrolled in school right now?"
  - "Grade 1: Anna, Barb, and Charlie. Grade 2: Alex, Peter and Zoe.
    Grade 3…"

This exercise was inspired by a pairing session with Phil Battos at [gSchool](http://gschool.it).

- remark: All our students only have one name. (It's a small town. What do you want?)
*/
public struct GradeSchool {

  /// A student's grade is an `Int`.
  public typealias Grade   = Int

  /// A student's name is a `String`.
  public typealias Student = String

  /// A dictionary from `Grade` to sorted array of `Student`s.
  public typealias Roster = [Grade: [Student]]

  /// A dictionary from `Grade` to `Set` of `Student`s.
  public typealias School = [Grade: Set<Student>]

  /// A `School`.
  public var roster: School

  /// A dictionary from `Grade` to sorted `Roster`.
  public var sortedRoster: Roster {
    return roster.reduce(Roster(minimumCapacity: roster.count)) { (var acc, roster) in
      let (grade, students) = roster
      acc[grade] = students.sort()
      return acc
    }
  }

  /// Initialize a `GradeSchool`, storing the empty `School` as `roster`.
  public init() {
    roster = School()
  }

  /**
  Given a `student` and a `grade`, append `student` to the array of `Student`s in `roster[grade]`.

  - parameter student: A `Student` in `grade`.
  - parameter grade: A `Grade`.

  - remark: If `roster[grade]` is `nil`, set it to `[student]`.
  */
  public mutating func addStudent(student: Student, grade: Grade) {
    var students = roster[grade] ?? Set<Student>()
    students.insert(student)
    roster[grade] = students
  }

  public func studentsInGrade(grade: Grade) -> Set<Student> {
    return roster[grade] ?? Set<Student>()
  }

}
