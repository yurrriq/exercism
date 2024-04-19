// Package atbash implements the Atbash exercise from Exercism.
package atbash

import (
	"strings"
	"unicode"
)

// Atbash encodes a given message using the Atbash cipher.
func Atbash(message string) string {
	var builder strings.Builder
	cursor := 0

	for _, roon := range message {
		if !(unicode.IsLetter(roon) || unicode.IsNumber(roon)) {
			continue
		}

		builder.WriteRune(ciper(roon))

		cursor++
		if cursor%5 == 0 {
			builder.WriteRune(' ')
		}
	}

	return strings.TrimSpace(builder.String())
}

func ciper(roon rune) rune {
	if !unicode.IsLetter(roon) {
		return roon
	}

	if unicode.IsUpper(roon) {
		return unicode.ToLower('Z' - (roon - 'A'))
	}

	return 'z' - (roon - 'a')
}
