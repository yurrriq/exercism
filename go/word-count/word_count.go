// Package wordcount implements [the Word Count exercise] from Exercism.
//
// [the Word Count exercise]: https://exercism.org/tracks/go/exercises/word-count
package wordcount

import (
	"regexp"
	"strings"
)

// WordRegexp is a regular expression that matches a word, i.e. one or more word
// characters, possibly followed by an apostrophe and one or more word
// characters.
var WordRegexp = regexp.MustCompile(`\w+(?:'\w+)?`)

// Frequency is a map from word to the number of occurrences.
type Frequency map[string]int

// WordCount computes the [Frequency] of words in a given phrase.
func WordCount(phrase string) Frequency {
	words := WordRegexp.FindAllString(strings.ToLower(phrase), -1)
	frequency := Frequency{}
	for _, word := range words {
		frequency[word]++
	}
	return frequency
}
