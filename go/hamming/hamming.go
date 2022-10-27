package hamming

import "errors"

func Distance(a, b string) (distance int, err error) {
	if len(a) != len(b) {
		return 0, errors.New("Can't compute Hamming distance for strings of unequal length")
	}

	for i, aRune := range a {
		if aRune != []rune(b)[i] {
			distance++
		}
	}

	return distance, nil
}
