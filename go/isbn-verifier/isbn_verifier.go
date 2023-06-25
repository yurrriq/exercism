// Package isbn implements the ISBN Verifier exercise.
package isbn

import (
	"unicode"
)

// IsValidISBN validates a book identification number.
func IsValidISBN(isbn string) bool {
	checksum := 0
	k := 10
	for i, roon := range []rune(isbn) {
		if roon == '-' && (i == 1 || i == 5 || i == 11) {
			continue
		}

		if 'X' == roon && k == 1 {
			checksum += 10 * k
			k--
			continue
		}

		if !unicode.IsDigit(roon) || k < 1 {
			return false
		}

		checksum += (int(roon) - '0') * k
		k--
	}

	return (k == 0) && checksum%11 == 0
}
