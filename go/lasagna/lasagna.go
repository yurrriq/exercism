package lasagna

const OvenTime int = 40

// Calculate the minutes remaining, based on the actual minutes already in the oven.
func RemainingOvenTime(actualMinutesInOven int) int {
	return OvenTime - actualMinutesInOven
}

// Calculate the time needed to prepare the lasagna, based on the amount of layers.
func PreparationTime(numberOfLayers int) int {
	return 2 * numberOfLayers
}

// Calculate the total time needed to create and bake a lasagna.
func ElapsedTime(numberOfLayers, actualMinutesInOven int) int {
	return PreparationTime(numberOfLayers) + actualMinutesInOven
}
