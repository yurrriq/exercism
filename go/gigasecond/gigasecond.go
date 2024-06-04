// Package gigasecond implements the [Gigasecond] exercise.
//
// [Gigasecond]: https://exercism.org/tracks/go/exercises/gigasecond
package gigasecond

import (
	"time"
)

// Gigasecond is a billion seconds in nanoseconds.
const Gigasecond time.Duration = 1e9 * 1e9

// AddGigasecond returns the time.Time a gigasecond later than a given
// time.Time.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(Gigasecond)
}
