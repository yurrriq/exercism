// Package anagram implements the Anagram exercise from Exercism.
package anagram

import (
	"sort"
	"strings"
)

// Detect returns the subset of candidates that are anagrams of a given target.
func Detect(target string, candidates []string) (anagrams []string) {
	loweredTarget, loweredSortedTarget := normalize(target)
	for _, candidate := range candidates {
		loweredCandidate, loweredSortedCandidate := normalize(candidate)
		if isAnagram(loweredTarget, loweredSortedTarget, loweredCandidate, loweredSortedCandidate) {
			anagrams = append(anagrams, candidate)
		}
	}
	return
}

func isAnagram(loweredLeft, loweredSortedLeft, loweredRight, loweredSortedRight string) bool {
	return loweredLeft != loweredRight && loweredSortedLeft == loweredSortedRight
}

func normalize(word string) (string, string) {
	lowered := strings.ToLower(word)
	loweredSorted := []rune(lowered)
	sort.Slice(loweredSorted, func(i, j int) bool {
		return loweredSorted[i] < loweredSorted[j]
	})
	return lowered, string(loweredSorted)
}
