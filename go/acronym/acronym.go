// Package acronym implements the Acronym exercise.
package acronym

import (
	"unicode"
)

// Abbreviate converts a longName into its acronym.
func Abbreviate(longName string) string {
	if len(longName) == 0 {
		return longName
	}

	roons := []rune(longName)
	acronym := []rune{unicode.ToUpper(roons[0])}
	for i, roon := range roons[1:] {
		if isWordStart(roon, roons[i]) {
			acronym = append(acronym, unicode.ToUpper(roon))
		}
	}

	return string(acronym)
}

func isWordStart(current rune, previous rune) bool {
	return unicode.IsLetter(current) && previous != '\'' && !unicode.IsLetter(previous)
}
