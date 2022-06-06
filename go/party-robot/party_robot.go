package partyrobot

import (
	"fmt"
	"strings"
)

// Greet a person by name.
func Welcome(name string) string {
	return fmt.Sprintf("Welcome to my party, %s!", name)
}

// Wish happy birthday to a person and exclaim their age.
func HappyBirthday(name string, age int) string {
	return fmt.Sprintf("Happy birthday %s! You are now %d years old!", name, age)
}

// Assign a table to each guest.
func AssignTable(name string, table int, neighbor, direction string, distance float64) string {
	return strings.Join([]string{
		fmt.Sprintf("Welcome to my party, %s!", name),
		fmt.Sprintf("You have been assigned to table %03d. Your table is %s, exactly %0.1f meters from here.", table, direction, distance),
		fmt.Sprintf("You will be sitting next to %s.", neighbor),
	}, "\n")
}
