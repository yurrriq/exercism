// Package lsproduct implements the [Largest Series Product] exercise from
// Exercism.
//
// [Largest Series Product]: https://exercism.org/tracks/go/exercises/largest-series-product
package lsproduct

import (
	"strconv"
)

const (
	// ErrSpanNegative is returned when a span is negative.
	ErrSpanNegative LSPError = "span must not be negative"

	// ErrSpanTooLarge is returned when a span is larger than the string length.
	ErrSpanTooLarge LSPError = "span must not be larger than string length"

	// ErrInvalidDigit is returned when input contains a nondigit.
	ErrInvalidDigit LSPError = "digits input must contain only digits"
)

// An LSPError is a string.
type LSPError string

// Error describes an LSPError as a string.
func (err LSPError) Error() string {
	return string(err)
}

// LargestSeriesProduct computes the largest product of a series of span digits.
func LargestSeriesProduct(digits string, span int) (int64, error) {
	if span == 0 {
		return 1, nil
	}

	if span < 0 {
		return 0, ErrSpanNegative
	}

	if span > len(digits) {
		return 0, ErrSpanTooLarge
	}

	var maxProduct int64
	for _, chunk := range chunksOf([]rune(digits), span) {
		var spanProduct int64 = 1
		for _, digit := range chunk {
			i64, err := strconv.ParseInt(string(digit), 10, 64)
			if err != nil {
				return 0, ErrInvalidDigit
			}
			spanProduct *= i64
		}

		if maxProduct < spanProduct {
			maxProduct = spanProduct
		}
	}

	return maxProduct, nil
}

func chunksOf(roons []rune, n int) [][]rune {
	chunks := [][]rune{}
	for i := range roons[:len(roons)-n+1] {
		chunks = append(chunks, roons[i:i+n])
	}
	return chunks
}
