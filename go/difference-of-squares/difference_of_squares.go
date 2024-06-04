//go:build !closedform

// Package diffsquares implements the [Difference of Squares] exercise.
//
// [Difference of Squares]: https://exercism.org/tracks/go/exercises/difference-of-squares
package diffsquares

// SquareOfSum returns the square of the sum of the first n natural numbers.
func SquareOfSum(n int) int {
	sum := 0
	for i := 1; i <= n; i++ {
		sum += i
	}

	return sum * sum
}

// SumOfSquares returns the sum of the squares of the first n natural numbers.
func SumOfSquares(n int) int {
	sum := 0
	for i := 1; i <= n; i++ {
		sum += i * i
	}

	return sum
}

// Difference returns the difference between the square of the sum of the first
// n numbers and the sum of the squares of the first n natural numbers.
func Difference(n int) int {
	return SquareOfSum(n) - SumOfSquares(n)
}
