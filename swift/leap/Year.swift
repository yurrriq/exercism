//
//  Year.swift
//  exercism
//
//  Created by Eric Bailey on 6/29/15.
//
//

class Year {
  var year: Int

  init(calendarYear: Int) {
    year = calendarYear
  }

  var isLeapYear: Bool {
    return (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
  }
}
