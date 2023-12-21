// Package proverb implements the proverb exercise from Exercism.
package proverb

import (
	"fmt"
)

// Proverb should have a comment documenting it.
func Proverb(rhyme []string) []string {
	k := len(rhyme)
	lines := make([]string, k)
	for i, want := range rhyme {
		if i == k-1 {
			lines[i] = fmt.Sprintf("And all for the want of a %s.", rhyme[0])
		} else {
			lines[i] = fmt.Sprintf("For want of a %s the %s was lost.", want, rhyme[i+1])
		}
	}
	return lines
}
