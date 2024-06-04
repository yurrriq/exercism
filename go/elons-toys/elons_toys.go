// Package elon implements the [Elon's Toys] exercise.
//
// [Elon's Toys]: https://exercism.org/tracks/go/exercises/elons-toys
package elon

import (
	"fmt"
)

// Drive updates the number of meters driven based on the car's speed, and
// reduces the battery according to the battery drainage.
func (car *Car) Drive() {
	if car.battery >= car.batteryDrain {
		car.battery -= car.batteryDrain
		car.distance += car.speed
	}
}

// DisplayDistance returns the distance as displayed on the LED display.
func (car *Car) DisplayDistance() string {
	return fmt.Sprintf("Driven %d meters", car.distance)
}

// DisplayBattery returns the battery percentage as displayed on the LED
// display.
func (car *Car) DisplayBattery() string {
	return fmt.Sprintf("Battery at %d%%", car.battery)
}

// CanFinish checks if a car is able to finish a certain track.
func (car *Car) CanFinish(trackDistance int) bool {
	return (car.battery/car.batteryDrain)*car.speed >= trackDistance
}
