// Package series implements [the Series exercise] on Exercism.
//
// [the Series exercise]: https://exercism.org/tracks/go/exercises/series
package series

// All returns all substrings of of s with length n.
func All(n int, s string) []string {
	if len(s) < n {
		return []string{}
	}

	return append([]string{UnsafeFirst(n, s)}, All(n, s[1:])...)
}

// UnsafeFirst returns the first substring of s with length n.
func UnsafeFirst(n int, s string) string {
	return s[:n]
}
