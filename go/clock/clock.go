// Package clock implements the Clock exercise.
package clock

import (
	"fmt"
)

// A Clock has integers Hours (modulo 24) and Minutes (modulo 60).
type Clock struct {
	Hours   int
	Minutes int
}

// New creates a new Clock from given hours and minutes.
func New(h, m int) Clock {
	return (Clock{Hours: 0, Minutes: 0}).Add(h*60 + m)
}

// Add a given number of minutes to a Clock.
func (c Clock) Add(m int) Clock {
	mq, minutes := divide(c.Minutes+m, 60)
	_, hours := divide(c.Hours+mq, 24)
	return Clock{Hours: hours, Minutes: minutes}
}

// Subtract a given number of minutes from a Clock.
func (c Clock) Subtract(m int) Clock {
	return c.Add(-m)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.Hours, c.Minutes)
}

// Euclidean division
func divide(dividend, divisor int) (quotient, remainder int) {
	if divisor == 0 {
		panic("Division by zero")
	}
	if divisor < 0 {
		quotient, remainder = divide(dividend, -divisor)
		quotient *= -1
		return
	}
	if dividend < 0 {
		quotient, remainder = divide(-dividend, divisor)
		if remainder == 0 {
			quotient *= -1
			return
		}
		return -quotient - 1, divisor - remainder
	}

	return divideUnsigned(dividend, divisor)
}

// Unchecked Euclidean division
func divideUnsigned(dividend, divisor int) (quotient, remainder int) {
	remainder = dividend
	if remainder < 0 {
		remainder += divisor
	}

	for remainder >= divisor {
		quotient++
		remainder -= divisor
	}
	return
}
