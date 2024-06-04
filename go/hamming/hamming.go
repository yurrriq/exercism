// Package hamming implements the [Hamming] exercise.
//
// [Hamming]: https://exercism.org/tracks/go/exercises/hamming
package hamming

// ErrInvalidStrands is returned when trying to compute the Hamming distance
// between two strands of unequal length.
const ErrInvalidStrands HammingError = "Can't compute Hamming distance for strands of unequal length"

// A DistanceError is a string.
type DistanceError string

func (err DistanceError) Error() string {
	return string(err)
}

// Distance calculates the Hamming distance between two DNA strands.
func Distance(strandA, strandB string) (distance int, err error) {
	if len(strandA) != len(strandB) {
		return -1, ErrInvalidStrands
	}

	for i := range strandA {
		if strandA[i] != strandB[i] {
			distance++
		}
	}

	return distance, nil
}
