// Package darts implements the Darts exercise.
package darts

const (
	innerRingSquared  = 1.0
	middleRingSquared = 25.0
	outerRingSquared  = 100.0
)

// Score takes the x and y coordinates of a point in the target and returns the
// number of points earned by a dart landing at that point.
func Score(x, y float64) int {
	distanceSquared := x*x + y*y
	switch {
	case distanceSquared > outerRingSquared:
		return 0
	case distanceSquared > middleRingSquared:
		return 1
	case distanceSquared > innerRingSquared:
		return 5
	default:
		return 10
	}
}
