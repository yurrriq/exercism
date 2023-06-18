// Package luhn implements the Luhn exercise.
package luhn

import (
	"unicode"
)

// Valid implements the Luhn algorithm to validate an identification number.
func Valid(id string) bool {
	var length, sum int
	roons := []rune(id)
	for i := len(roons) - 1; i >= 0; i-- {
		roon := roons[i]
		switch {
		case unicode.IsSpace(roon):
			continue
		case !unicode.IsDigit(roon):
			return false
		default:
			digit := int(roon) - '0'
			if length&1 == 1 {
				digit *= 2
			}
			if digit > 9 {
				digit -= 9
			}
			sum += digit
			length++
		}
	}
	return length > 1 && sum%10 == 0
}
