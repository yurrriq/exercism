package purchase

import "fmt"

// Determine whether a license is needed to drive a given kind of vehicle.
// Only "car" and "truck" require a license.
func NeedsLicense(kind string) bool {
	return kind == "car" || kind == "truck"
}

// Recommend a vehicle for selection.
// Always recommend the vehicle that comes first in lexicographical order.
func ChooseVehicle(option1, option2 string) string {
	var choice string
	if option1 < option2 {
		choice = option1
	} else {
		choice = option2
	}
	return fmt.Sprintf("%s is clearly the better choice.", choice)
}

// Calculate how much a vehicle can resell for at a certain age.
func CalculateResellPrice(originalPrice, age float64) float64 {
	var currentValue float64
	if age < 3 {
		currentValue = 0.8
	} else if age < 10 {
		currentValue = 0.7
	} else {
		currentValue = 0.5
	}

	return originalPrice * currentValue
}
