// Package anagram implements the Anagram exercise from Exercism.
package anagram

import (
	"sort"
	"strings"
)

// Detect returns the subset of candidates that are anagrams of a given target.
func Detect(target string, candidates []string) (anagrams []string) {
	targetLowered := strings.ToLower(target)
	targetSortedRunes := sortedRunes(targetLowered)

	for _, candidate := range candidates {
		if isAnagram(targetLowered, targetSortedRunes, candidate) {
			anagrams = append(anagrams, candidate)
		}
	}
	return
}

func isAnagram(targetLowered string, targetSortedRunes []rune, candidate string) bool {
	candidateLowered := strings.ToLower(candidate)
	if targetLowered == candidateLowered {
		return false
	}

	candidateSortedRunes := sortedRunes(candidateLowered)
	if len(targetSortedRunes) != len(candidateSortedRunes) {
		return false
	}

	for i, r := range targetSortedRunes {
		if candidateSortedRunes[i] != r {
			return false
		}
	}

	return true
}

func sortedRunes(word string) []rune {
	sorted := []rune(word)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i] < sorted[j]
	})
	return sorted
}
