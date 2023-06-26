// Package clock implements the Clock exercise.
package clock

import (
	"fmt"
)

const (
	dayMinutes  = 1440
	hourMinutes = 60
)

// A Clock is a number of minutes (modulo 1440).
type Clock struct {
	minutes int
}

// New creates a new Clock from given hours and minutes.
func New(h, m int) Clock {
	return Clock{0}.Add(h*hourMinutes + m)
}

// Add a given number of minutes to a Clock.
func (c Clock) Add(m int) Clock {
	minutes := (c.minutes + m) % dayMinutes

	if minutes < 0 {
		minutes += dayMinutes
	}

	return Clock{minutes}
}

// Subtract a given number of minutes from a Clock.
func (c Clock) Subtract(m int) Clock {
	return c.Add(-m)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.minutes/hourMinutes, c.minutes%hourMinutes)
}
