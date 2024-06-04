// Package birdwatcher implements the [Bird Watcher] exercise.
//
// [Bird Watcher]: https://exercism.org/tracks/go/exercises/bird-watcher
package birdwatcher

// TotalBirdCount returns the total bird count by summing the individual days'
// counts.
func TotalBirdCount(birdsPerDay []int) int {
	var count int
	for _, birds := range birdsPerDay {
		count += birds
	}

	return count
}

// BirdsInWeek returns the bird count by summing only the counts from the given
// week.
func BirdsInWeek(birdsPerDay []int, week int) int {
	start := (week - 1) * 7
	return TotalBirdCount(birdsPerDay[start : start+7])
}

// FixBirdCountLog returns the bird counts after correcting for the one bird
// that was hiding in a far corner of the garden every second day.
func FixBirdCountLog(birdsPerDay []int) []int {
	for i := range birdsPerDay {
		if i%2 == 0 {
			birdsPerDay[i]++
		}
	}

	return birdsPerDay
}
