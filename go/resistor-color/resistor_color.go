// Package resistorcolor implements the Resistor Color exercise.
package resistorcolor

// A ResistorColor is a color code to denote a resistance value.
type ResistorColor int

// ResistorColors
const (
	Black ResistorColor = iota
	Brown
	Red
	Orange
	Yellow
	Green
	Blue
	Violet
	Grey
	White
)

func (color ResistorColor) String() string {
	return Colors()[color]
}

// Colors returns the list of all colors.
func Colors() []string {
	return []string{"black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"}
}

// ColorCode returns the resistance value of the given color.
func ColorCode(color string) int {
	var code ResistorColor
	for code = Black; code <= White; code++ {
		if color == code.String() {
			return int(code)
		}
	}

	return -1
}
