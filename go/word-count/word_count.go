// Package wordcount implements the Word Count exercism from Exercism.
package wordcount

import (
	"regexp"
	"strings"
)

// Frequency is a map from word to the number of occurrences.
type Frequency map[string]int

// WordCount computes the frequency of words in a given a phrase.
func WordCount(phrase string) Frequency {
	nonAlphanumeric := regexp.MustCompile(`[^[:alpha:][:digit:]']+`)
	words := nonAlphanumeric.Split(strings.ToLower(phrase), -1)
	frequency := Frequency{}
	for _, word := range words {
		normalizedWord := strings.Trim(strings.ToLower(word), `'`)
		if len(normalizedWord) == 0 {
			continue
		}
		frequency[normalizedWord]++
	}
	return frequency
}
