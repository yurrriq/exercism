package pangram

import (
	"unicode"
)

func IsPangram(input string) bool {
	seen := make(map[rune]struct{}, 26)

	for _, letter := range input {
		letter = unicode.ToLower(letter)
		if _, ok := seen[letter]; !ok {
			seen[letter] = struct{}{}
		}
	}

	for letter := 'a'; letter <= 'z'; letter++ {
		if _, ok := seen[letter]; !ok {
			return false
		}
	}

	return true
}
