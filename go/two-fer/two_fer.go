// Package twofer implements the Two Fer exercise.
package twofer

// Given a name, return a string with the message, "One for name, one for me."
func ShareWith(name string) string {
	if name == "" {
		return ShareWith("you")
	}

	return "One for " + name + ", one for me."
}
