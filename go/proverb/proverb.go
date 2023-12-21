// Package proverb implements the proverb exercise from Exercism.
package proverb

import (
	"fmt"
)

const (
	lineIntermediate string = "For want of a %s the %s was lost."
	lineLast         string = "And all for the want of a %s."
)

// Proverb takes a list of inputs and returns the relevant proverb.
func Proverb(rhyme []string) []string {
	k := len(rhyme)
	if k == 0 {
		return []string{}
	}

	lines := make([]string, k)
	lines[k-1] = fmt.Sprintf(lineLast, rhyme[0])

	for i, want := range rhyme[:k-1] {
		lines[i] = fmt.Sprintf(lineIntermediate, want, rhyme[i+1])
	}

	return lines
}
