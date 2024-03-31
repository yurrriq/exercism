// Package dndcharacter implements the [D&D Character] exercise from Exercism.
//
// [D&D Character]: https://exercism.org/tracks/go/exercises/dnd-character
package dndcharacter

import (
	"math"
	"math/rand"
)

// Character represents a D&D character with six abilities and hit points.
type Character struct {
	Strength     int
	Dexterity    int
	Constitution int
	Intelligence int
	Wisdom       int
	Charisma     int
	Hitpoints    int
}

// Modifier calculates the ability modifier for a given ability score
func Modifier(score int) int {
	return int(math.Floor(float64(score-10) / 2.0))
}

// Ability uses randomness to generate the score for an ability
func Ability() (score int) {
	lowestRoll := rollDie(6)
	for i := 0; i < 3; i++ {
		roll := rollDie(6)
		if roll < lowestRoll {
			score += lowestRoll
			lowestRoll = roll
		} else {
			score += roll
		}
	}

	return score
}

// GenerateCharacter creates a new Character with random scores for abilities
func GenerateCharacter() (character Character) {
	character.Strength = Ability()
	character.Dexterity = Ability()
	character.Constitution = Ability()
	character.Intelligence = Ability()
	character.Wisdom = Ability()
	character.Charisma = Ability()
	character.Hitpoints = 10 + Modifier(character.Constitution)

	return character
}

// rollDie simulates rolling a d-sided die.
func rollDie(d int) int {
	return rand.Intn(d) + 1
}
