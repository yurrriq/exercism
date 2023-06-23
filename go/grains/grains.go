// Package grains implements the Grains exercise.
package grains

// ErrInvalidSquare is returned when a given square is not with 1-64 inclusive.
const ErrInvalidSquare SquareError = "Invalid square"

// A SquareError is a string.
type SquareError string

// Error describes a SquareError as a string.
func (err SquareError) Error() string {
	return string(err)
}

// Square returns how many grains were on a given square.
func Square(square int) (uint64, error) {
	if 1 > square || square > 64 {
		return 0, ErrInvalidSquare
	}

	return 1 << (square - 1), nil
}

// Total returns the total number of grains on the chessboard.
func Total() uint64 {
	return 1<<64 - 1
}
