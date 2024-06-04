// Package gross implements a point of sale (POS) system for the [Gross Store]
// exercise.
//
// [Gross Store]: https://exercism.org/tracks/go/exercises/gross-store
package gross

// Units stores the Gross Store unit measurements.
func Units() map[string]int {
	return map[string]int{
		"quarter_of_a_dozen": 3,
		"half_of_a_dozen":    6,
		"dozen":              12,
		"small_gross":        120,
		"gross":              144,
		"great_gross":        1728,
	}
}

// NewBill creates a new bill.
func NewBill() map[string]int {
	return map[string]int{}
}

// AddItem adds an item to a customer's bill.
func AddItem(bill, units map[string]int, item, unit string) bool {
	newUnit, isKnownUnit := units[unit]
	if isKnownUnit {
		bill[item] += newUnit
	}

	return isKnownUnit
}

// RemoveItem removes an item from a customer's bill.
func RemoveItem(bill, units map[string]int, item, unit string) bool {
	oldUnit, isPresent := bill[item]
	if !isPresent {
		return false
	}

	newUnit, isKnownUnit := units[unit]
	if !isKnownUnit || newUnit > oldUnit {
		return false
	}

	if newUnit == oldUnit {
		delete(bill, item)
	} else {
		bill[item] -= newUnit
	}

	return true
}

// GetItem returns the quantity of an item that the customer has in their bill.
func GetItem(bill map[string]int, item string) (int, bool) {
	quantity, isPresent := bill[item]
	if !isPresent {
		return 0, false
	}

	return quantity, isPresent
}
