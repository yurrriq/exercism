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
	for _, n := range ints {
		if pred(n) {
			filteredInts = append(filteredInts, n)
		}
	}

	return filteredInts
}

// Discard takes a int predicate and returns a slice of only those given ints
// for which the predicate does not hold.
func (ints Ints) Discard(pred func(int) bool) (filteredInts Ints) {
	for _, n := range ints {
		if !pred(n) {
			filteredInts = append(filteredInts, n)
		}
	}

	return filteredInts
}

// Keep takes a []int predicate and returns a slice of only those given []int
// for which the predicate holds.
func (lists Lists) Keep(pred func([]int) bool) (filteredLists Lists) {
	for _, list := range lists {
		if pred(list) {
			filteredLists = append(filteredLists, list)
		}
	}

	return filteredLists
}

// Keep takes a string predicate and returns a slice of only those given strings
// for which the predicate holds.
func (strings Strings) Keep(pred func(string) bool) (filteredStrings Strings) {
	for _, string := range strings {
		if pred(string) {
			filteredStrings = append(filteredStrings, string)
		}
	}

	return filteredStrings
}
