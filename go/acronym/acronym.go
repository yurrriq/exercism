// Package acronym implements the Acronym exercise.
package acronym

import (
	"strings"
	"unicode"
)

// Abbreviate converts a longName into its acronym.
func Abbreviate(longName string) string {
	previous := ' '
	return strings.Map(
		func(current rune) rune {
			if isWordStart(current, previous) {
				previous = current
				return unicode.ToTitle(current)
			}
			previous = current
			return -1
		},
		longName)
}

func isWordStart(current rune, previous rune) bool {
	return unicode.IsLetter(current) && previous != '\'' && !unicode.IsLetter(previous)
}
