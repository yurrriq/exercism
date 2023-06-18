// Package resistorcolor implements the Resistor Color exercise.
package resistorcolor

// Colors returns the list of all colors.
func Colors() []string {
	return []string{"black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"}
}

// ColorCode returns the resistance value of the given color.
func ColorCode(color string) int {
	for code, codedColor := range Colors() {
		if color == codedColor {
			return code
		}
	}

	return -1
}
