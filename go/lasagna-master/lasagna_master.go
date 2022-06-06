package lasagna

// Calculate the time needed to prepare the lasagna, based on the amount of
// layers and preparation time in minutes per layer.
func PreparationTime(layers []string, timePerLayernumberOfLayers int) int {
	if timePerLayernumberOfLayers == 0 {
		return 2 * len(layers)
	} else {
		return timePerLayernumberOfLayers * len(layers)
	}
}

// Compute the amounts of noodles and sauce needed.
func Quantities(layers []string) (int, float64) {
	var noodles int = 0
	var sauce float64 = 0.
	for _, layer := range layers {
		if layer == "noodles" {
			noodles += 50
		} else if layer == "sauce" {
			sauce += 0.2
		}
	}

	return noodles, sauce
}

// Add the secret ingredient, i.e. the ingredient on the friend's list.
func AddSecretIngredient(friendsList []string, myList []string) []string {
	myList[len(myList)-1] = friendsList[len(friendsList)-1]
	return myList
}

// Given the quantities needed for two portions, scale the recipe to the desired
// number of portions.
func ScaleRecipe(quantities []float64, portions int) []float64 {
	scaledQuantities := make([]float64, len(quantities))
	for i, quantity := range quantities {
		scaledQuantities[i] = (quantity / 2.0) * float64(portions)
	}

	return scaledQuantities
}
