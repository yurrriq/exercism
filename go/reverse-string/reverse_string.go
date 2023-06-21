// Package reverse implements the Reverse String exercise.
package reverse

// Reverse a string.
func Reverse(input string) string {
	runes := []rune(input)
	length := len(runes)
	for i := 0; i < length/2; i++ {
		runes[i], runes[length-1-i] = runes[length-1-i], runes[i]
	}

	return string(runes)
}
