package hamming

const ErrInvalidStrands HammingError = "Can't compute Hamming distance for strands of unequal length"

type HammingError string

func (err HammingError) Error() string {
	return string(err)
}

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
