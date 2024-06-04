// Package collatzconjecture implements the [Collatz Conjecture] exercise.
//
// [Collatz Conjecture]: https://exercism.org/tracks/go/exercises/collatz-conjecture
package collatzconjecture

// ErrInvalidNumber represents an error when a nonpositive number is given.
const ErrInvalidNumber CollatzError = "Only strictly positive numbers are allowed"

// A CollatzError is a string.
type CollatzError string

func (err CollatzError) Error() string {
	return string(err)
}

// CollatzConjecture return the number of steps required to reach 1 from a given
// number.
func CollatzConjecture(n int) (int, error) {
	if n <= 0 {
		return -1, ErrInvalidNumber
	}

	var steps int
	for steps = 0; n != 1; steps++ {
		if 0 == n&1 {
			n /= 2
		} else {
			n *= 3
			n++
		}
	}

	return steps, nil
}
