// Package series implements [the Series exercise] on Exercism.
//
// [the Series exercise]: https://exercism.org/tracks/go/exercises/series
package series

// All returns all substrings of of s with length n.
func All(n int, s string) []string {
	substrings := make([]string, 0, len(s)-n+1)
	for i := 0; i <= len(s)-n; i++ {
		substrings = append(substrings, s[i:i+n])
	}

	return substrings
}

// First returns the first substring of s with length n, when possible.
func First(n int, s string) (first string, ok bool) {
	if n > len(s) {
		return s, false
	}
	return UnsafeFirst(n, s), true
}

// UnsafeFirst returns the first substring of s with length n.
func UnsafeFirst(n int, s string) string {
	return s[:n]
}
