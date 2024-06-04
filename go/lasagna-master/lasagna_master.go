// Package lasagna implements the [Lasagna Master] exercism from Exercism.
//
// [Lasagna Master]: https://exercism.org/tracks/go/exercises/lasagna-master
package lasagna

// PreparationTime calculates the time needed to prepare the lasagna, based on
// the amount of layers and preparation time in minutes per layer.
func PreparationTime(layers []string, timePerLayernumberOfLayers int) int {
	if timePerLayernumberOfLayers == 0 {
		return 2 * len(layers)
	}

	return timePerLayernumberOfLayers * len(layers)
}

// Quantities computes the amounts of noodles and sauce needed.
func Quantities(layers []string) (int, float64) {
	var noodles int
	var sauce float64
	for _, layer := range layers {
		if layer == "noodles" {
			noodles += 50
		} else if layer == "sauce" {
			sauce += 0.2
		}
	}

	return noodles, sauce
}

// AddSecretIngredient adds the secret ingredient, i.e. the ingredient on the
// friend's list.
func AddSecretIngredient(friendsList []string, myList []string) []string {
	myList[len(myList)-1] = friendsList[len(friendsList)-1]
	return myList
}

// ScaleRecipe scales a recipe to the desired number of portions, given the
// quantities needed for two portions.
func ScaleRecipe(quantities []float64, portions int) []float64 {
	scaledQuantities := make([]float64, len(quantities))
	for i, quantity := range quantities {
		scaledQuantities[i] = (quantity / 2.0) * float64(portions)
	}

	return scaledQuantities
}
