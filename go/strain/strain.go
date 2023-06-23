// Package strain implements the Strain exercise.
package strain

// Ints is a slice of ints.
type Ints []int

// Lists is a slice of slices of ints.
type Lists [][]int

// Strings is a slice of strings.
type Strings []string

// Keep takes a int predicate and returns a slice of only those given ints for
// which the predicate holds.
func (ints Ints) Keep(pred func(int) bool) (filteredInts Ints) {
	return keep(pred, ints)
}

// Discard takes a int predicate and returns a slice of only those given ints
// for which the predicate does not hold.
func (ints Ints) Discard(pred func(int) bool) (filteredInts Ints) {
	return discard(pred, ints)
}

// Keep takes a []int predicate and returns a slice of only those given []int
// for which the predicate holds.
func (lists Lists) Keep(pred func([]int) bool) (filteredLists Lists) {
	return keep(pred, lists)
}

// Keep takes a string predicate and returns a slice of only those given strings
// for which the predicate holds.
func (strings Strings) Keep(pred func(string) bool) (filteredStrings Strings) {
	return keep(pred, strings)
}

func keep[S ~[]E, E any](pred func(E) bool, slice S) (filtered S) {
	for _, element := range slice {
		if pred(element) {
			filtered = append(filtered, element)
		}
	}

	return filtered
}

func discard[S ~[]E, E any](pred func(E) bool, slice S) (filtered S) {
	return keep(negate(pred), slice)
}

func negate[E any](f func(E) bool) func(E) bool {
	return func(x E) bool {
		return !f(x)
	}
}
