package luhn

import (
	"unicode"
)

func Valid(id string) bool {
	runes, length, sum := []rune(id), 0, 0
	for i := len(runes) - 1; i >= 0; i-- {
		rune := runes[i]
		switch {
		case unicode.IsSpace(rune):
			continue
		case !unicode.IsDigit(rune):
			return false
		default:
			digit := int(rune) - '0'
			if (length & 1) == 1 {
				digit *= 2
			}
			if digit > 9 {
				digit -= 9
			}
			length++
			sum += digit
		}
	}

	return (length > 1) && (sum%10 == 0)
}
