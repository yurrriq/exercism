package scrabble

import "strings"

var lettersScoreMap = map[string]int{
	"AEIOULNRST": 1,
	"DG":         2,
	"BCMP":       3,
	"FHVWY":      4,
	"K":          5,
	"JX":         8,
	"QZ":         10,
}

func Score(word string) (score int) {
WordLetterIterator:
	for _, wordLetter := range strings.ToUpper(word) {
		for letters, letterScore := range lettersScoreMap {
			for _, letter := range letters {
				if wordLetter == letter {
					score += letterScore
					continue WordLetterIterator
				}
			}
		}
	}

	return score
}
