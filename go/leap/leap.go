// Determine whether a year is a leap year.
package leap

// Determine whether a given year is a leap year.
func IsLeapYear(year int) bool {
	return (year%400 == 0) || ((year%4 == 0) && !(year%100 == 0))
}
