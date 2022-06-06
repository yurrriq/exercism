package birdwatcher

// Return the total bird count by summing the individual day's counts.
func TotalBirdCount(birdsPerDay []int) int {
	var count int = 0
	for _, birds := range birdsPerDay {
		count += birds
	}

	return count
}

// Return the bird count by summing only the items belonging to the given week.
func BirdsInWeek(birdsPerDay []int, week int) int {
	start := (week - 1) * 7
	return TotalBirdCount(birdsPerDay[start:start+7])
}

// Return the bird counts after correcting for alternate days.
func FixBirdCountLog(birdsPerDay []int) []int {
	for i := range birdsPerDay {
		if i % 2 == 0 {
			birdsPerDay[i] += 1
		}
	}

	return birdsPerDay
}
