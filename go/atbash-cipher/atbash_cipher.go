// Package atbash implements the Atbash exercise from Exercism.
package atbash

import (
	"strings"
	"unicode"
)

// Atbash encodes a given message using the Atbash cipher.
func Atbash(message string) string {
	var builder strings.Builder
	clusterLength := 0

	for _, roon := range message {
		if !(unicode.IsLetter(roon) || unicode.IsDigit(roon)) {
			continue
		}

		if clusterLength == 5 {
			builder.WriteRune(' ')
			clusterLength = 0
		}

		if unicode.IsLetter(roon) {
			roon = 'a' + 'z' - unicode.ToLower(roon)
		}

		builder.WriteRune(roon)
		clusterLength++
	}

	return builder.String()
}
