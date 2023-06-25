// Package resistorcolorduo implements the Resistor Color Duo exercise.
package resistorcolorduo

const ignoreBandsAfterSecond = true

// Value returns the resistance value of a resistor with given colors.
func Value(colors []string) int {
	length := len(colors)
	if length < 2 {
		return -1
	}

	if ignoreBandsAfterSecond {
		length = 2
	}

	resistances := make([]int, length)

decode:
	for resistance, color := range allColors() {
		for i, band := range colors {
			if ignoreBandsAfterSecond && i > 1 {
				continue decode
			}
			if band == color {
				resistances[i] = resistance
			}
		}
	}

	totalResistance := 0
	for i, resistance := range resistances {
		totalResistance += resistance * ipow(10, length-1-i)
	}

	return totalResistance
}

func allColors() []string {
	return []string{"black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"}
}

func ipow(base, exp int) int {
	if exp < 0 {
		panic("Negative exponent")
	}

	result := 1
	for {
		if exp%2 == 1 {
			result *= base
		}
		exp /= 2
		if exp == 0 {
			break
		}
		base *= base
	}

	return result
}
