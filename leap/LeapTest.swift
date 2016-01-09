import XCTest

class LeapTest: XCTestCase {

  func testVanillaLeapYear() {
    let year = Year(calendarYear: 1996)
    XCTAssert(year.isLeapYear)
  }

  func testAnyOldYear() {
    let year = Year(calendarYear: 1997)
    XCTAssert(!year.isLeapYear)
  }

  func testCentury() {
    let year = Year(calendarYear: 1900)
    XCTAssert(!year.isLeapYear)
  }

  func testExceptionalCentury() {
    let year = Year(calendarYear: 2400)
    XCTAssert(year.isLeapYear)
  }

}
