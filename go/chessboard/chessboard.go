// Package chessboard implements the [Chessboard] exercise.
//
// [Chessboard]: https://exercism.org/tracks/go/exercises/chessboard
package chessboard

// File represents whether squares in a vertical column are occupied by a piece.
type File []bool

// Chessboard is a map of eight Files, accessed with keys from "A" to "H".
type Chessboard map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(board Chessboard, file string) int {
	numOccupiedSquares := 0

	for _, squareIsOccupied := range board[file] {
		if squareIsOccupied {
			numOccupiedSquares++
		}
	}

	return numOccupiedSquares
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(board Chessboard, rank int) int {
	numOccupiedSquares := 0

	if rank >= 1 && rank <= 8 {
		for _, file := range board {
			if file[rank-1] {
				numOccupiedSquares++
			}
		}
	}

	return numOccupiedSquares
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(board Chessboard) int {
	numSquares := 0

	for _, file := range board {
		numSquares += len(file)
	}

	return numSquares
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(board Chessboard) int {
	numOccupiedSquares := 0

	for file := range board {
		numOccupiedSquares += CountInFile(board, file)
	}

	return numOccupiedSquares
}
