// Package lsproduct implements the [Largest Series Product] exercise from
// Exercism.
//
// [Largest Series Product]: https://exercism.org/tracks/go/exercises/largest-series-product
package lsproduct

import (
	"errors"
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
func LargestSeriesProduct(digits string, span int) (maxProduct int64, err error) {
	if span == 0 {
		maxProduct = 1
		return
	}

	if span < 0 {
		err = ErrSpanNegative
		return
	}

	if span > len(digits) {
		err = ErrSpanTooLarge
		return
	}

	chunks, err := ChunksOf([]rune(digits), span)
	if err != nil {
		return
	}

	for _, chunk := range chunks {
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

// ChunksOf splits a slice into chunks of chunkSize, which must be positive.
func ChunksOf[A any](slice []A, chunkSize int) (chunks [][]A, err error) {
	if chunkSize <= 0 {
		return nil, errors.New("chunkSize must be positive")
	}

	for i := range slice[:len(slice)-chunkSize+1] {
		chunks = append(chunks, slice[i:i+chunkSize])
	}

	return
}
