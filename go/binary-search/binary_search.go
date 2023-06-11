package binarysearch

import (
	"golang.org/x/exp/constraints"
)

// SearchInts searches for an int in a slice of ints by repeatedly splitting the
// slice in half, only keeping the half which contains desired int.
func SearchInts(haystack []int, needle int) int {
	index, _ := binarySearch(haystack, needle, 0)
	return index
}

func binarySearch[T constraints.Ordered](haystack []T, needle T, iterations int) (int, int) {
	length := len(haystack)
	if length == 0 {
		return -1, iterations
	}

	pivot := length / 2
	middle := haystack[pivot]

	switch {
	case needle < middle:
		return binarySearch(haystack[:pivot], needle, iterations+1)
	case needle > middle:
		index, iterations := binarySearch(haystack[pivot+1:], needle, iterations+1)
		if index >= 0 {
			return index + pivot + 1, iterations
		}
		return -1, iterations
	default:
		return pivot, iterations
	}
}
