// Package bob implements the Bob exercise.
package bob

import (
	"strings"
	"unicode"

	"golang.org/x/exp/slices"
)

// Hey responds to a given remark as a lackadaisical teenager would.
func Hey(remark string) string {
	trimmedRemark := strings.TrimFunc(remark, unicode.IsSpace)
	switch {
	case trimmedRemark == "":
		return "Fine. Be that way!"
	case isForcefulQuestion(trimmedRemark):
		return "Calm down, I know what I'm doing!"
	case isQuestion(trimmedRemark):
		return "Sure."
	case isYelled(trimmedRemark):
		return "Whoa, chill out!"
	default:
		return "Whatever."
	}
}

func isForcefulQuestion(remark string) bool {
	return isQuestion(remark) && isYelled(remark)
}

func isQuestion(remark string) bool {
	return strings.HasSuffix(remark, "?")
}

func isYelled(remark string) bool {
	return slices.ContainsFunc([]rune(remark), unicode.IsLetter) &&
		remark == strings.ToUpper(remark)
}
