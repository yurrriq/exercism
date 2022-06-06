package cards

// Return a slice with the cards 2, 6 and 9 in that order.
func FavoriteCards() []int {
	return []int{2, 6, 9}
}

// Retrieve an item from a slice at given position.
// If the index is out of range, return -1.
func GetItem(slice []int, index int) int {
	if index < 0 || index >= len(slice) {
		return -1
	} else {
		return slice[index]
	}
}

// Write an item to a slice at given position overwriting an existing value.
// If the index is out of range the value needs to be appended.
func SetItem(slice []int, index, value int) []int {
	if index < 0 || index >= len(slice) {
		return append(slice, value)
	} else {
		slice[index] = value
		return slice
	}
}

// Add an arbitrary number of values at the front of a slice.
func PrependItems(slice []int, value ...int) []int {
	return append(value, slice...)
}

// Remove an item from a slice by modifying the existing slice.
func RemoveItem(slice []int, index int) []int {
	if index < 0 || index >= len(slice) {
		return slice
	} else if index == 0 {
		return slice[1:]
	} else {
		return append(slice[:index], slice[index+1:]...)
	}
}
