// Package etl implements the ETL exercise.
package etl

import (
	"strings"
)

// Transform a mapping from score to list of letters (one-to-many) to a mapping
// from letter to score (one-to-one).
func Transform(input map[int][]string) map[string]int {
	transformed := make(map[string]int)
	for score, letters := range input {
		for _, letter := range letters {
			transformed[strings.ToLower(letter)] = score
		}
	}

	return transformed
}
