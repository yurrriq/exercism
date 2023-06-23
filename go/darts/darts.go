// Package darts implements the Darts exercise.
package darts

import (
	"math"
)

const (
	innerRing  = 1.0
	middleRing = 5.0
	outerRing  = 10.0
)

// Score takes the x and y coordinates of a point in the target and returns the
// number of points earned by a dart landing at that point.
func Score(x, y float64) (score int) {
	distance := math.Hypot(x, y)
	switch {
	case distance > outerRing:
		score = 0
	case distance > middleRing:
		score = 1
	case distance > innerRing:
		score = 5
	default:
		score = 10
	}

	return
}
