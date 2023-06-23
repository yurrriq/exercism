// Package bob implements the Bob exercise.
package bob

import (
	"strings"
	"unicode"
)

// Hey responds to a given remark as a lackadaisical teenager would.
func Hey(remark string) string {
	trimmedRemark := strings.TrimSpace(remark)

	if isEmpty(trimmedRemark) {
		return "Fine. Be that way!"
	}

	question := isQuestion(trimmedRemark)
	yelled := isYelled(trimmedRemark)

	switch {
	case question && yelled:
		return "Calm down, I know what I'm doing!"
	case question:
		return "Sure."
	case yelled:
		return "Whoa, chill out!"
	default:
		return "Whatever."
	}
}

func isEmpty(remark string) bool {
	return remark == ""
}

func isForcefulQuestion(remark string) bool {
	return isQuestion(remark) && isYelled(remark)
}

func isQuestion(remark string) bool {
	return strings.HasSuffix(remark, "?")
}

func isYelled(remark string) bool {
	return strings.IndexFunc(remark, unicode.IsLetter) != -1 &&
		strings.ToUpper(remark) == remark
}
