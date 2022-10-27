package isogram

import (
	mapset "github.com/deckarep/golang-set/v2"
	"unicode"
)

func IsIsogram(word string) bool {
	seenLetters := mapset.NewSet[rune]()

	for _, letter := range word {
		if !unicode.IsLetter(letter) {
			continue
		}

		letterUpper := unicode.ToUpper(letter)

		if seenLetters.Contains(letterUpper) {
			return false
		}

		seenLetters.Add(letterUpper)
	}

	return true
}
