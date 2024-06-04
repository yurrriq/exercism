// Package booking implements the [Booking up for Beauty] exercise.
//
// [Booking up for Beauty]: https://exercism.org/tracks/go/exercises/booking-up-for-beauty
package booking

import (
	"log"
	"time"
)

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) time.Time {
	time, err := time.Parse("1/2/2006 15:04:05", date)
	if err != nil {
		log.Panic(err)
	}

	return time
}

// HasPassed determines whether a given date has passed.
func HasPassed(date string) bool {
	now := time.Now().UTC()

	then, err := time.Parse("January 2, 2006 15:04:05", date)
	if err != nil {
		log.Panic(err)
	}

	return now.After(then)
}

// IsAfternoonAppointment determines whether a given time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	appointment, err := time.Parse("Monday, January 2, 2006 15:04:05", date)
	if err != nil {
		log.Panic(err)
	}

	hour := appointment.Hour()
	return hour >= 12 && hour < 18
}

// Description returns a formatted string of the given time.
func Description(date string) string {
	return Schedule(date).Format("You have an appointment on Monday, January 2, 2006, at 15:04.")
}

// AnniversaryDate returns a time.Time with this year's anniversary.
func AnniversaryDate() time.Time {
	return time.Date(time.Now().UTC().Year(), time.September, 15, 0, 0, 0, 0, time.UTC)
}
