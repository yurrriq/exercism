package elon

import "fmt"

func (car *Car) Drive() {
	if car.battery >= car.batteryDrain {
		car.battery -= car.batteryDrain
		car.distance += car.speed
	}
}

func (car *Car) DisplayDistance() string {
	return fmt.Sprintf("Driven %d meters", car.distance)
}

func (car *Car) DisplayBattery() string {
	return fmt.Sprintf("Battery at %d%%", car.battery)
}

// Check if a car is able to finish a certain track.
func (car *Car) CanFinish(trackDistance int) bool {
	segments := (trackDistance - car.distance) / car.speed
	return car.battery-(car.batteryDrain*segments) >= 0
}
