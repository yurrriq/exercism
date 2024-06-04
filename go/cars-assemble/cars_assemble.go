// Package cars implements the [Cars Assemble] exercise.
//
// [Cars Assemble]: https://exercism.org/tracks/go/exercises/cars-assemble
package cars

import "math/bits"

// CalculateWorkingCarsPerHour calculates how many working cars are produced by
// the assembly line every hour.
func CalculateWorkingCarsPerHour(productionRate int, successRate float64) float64 {
	return float64(productionRate) * (successRate / 100)
}

// CalculateWorkingCarsPerMinute calculates how many working cars are produced
// by the assembly line every minute.
func CalculateWorkingCarsPerMinute(productionRate int, successRate float64) int {
	workingCarsPerHour := CalculateWorkingCarsPerHour(productionRate, successRate)
	return int(workingCarsPerHour / 60)
}

// CalculateCost calculates the cost of producing the given number of cars.
func CalculateCost(carsCount int) uint {
	quotient, remainder := bits.Div(0, uint(carsCount), 10)
	return (quotient * 95000) + (remainder * 10000)
}
