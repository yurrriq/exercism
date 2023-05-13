// Calculating gigasecond anniversaries.
package gigasecond

import (
	"time"
)

// A gigasecond in nanoseconds.
const Gigasecond time.Duration = 1e9 * 1e9

// Given a time.Time, return the time.Time a gigasecond later.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(Gigasecond)
}
