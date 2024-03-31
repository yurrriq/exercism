// Package dndcharacter implements the [D&D Character] exercise from Exercism.
//
// [D&D Character]: https://exercism.org/tracks/go/exercises/dnd-character
package dndcharacter

import (
	"math"
	"math/rand"
	"time"
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
	src := rand.NewSource(time.Now().UnixNano())
	rng := rand.New(src)

	lowestRoll := math.MaxInt
	for i := 0; i < 4; i++ {
		roll := rollDie(rng, 6)
		score += roll
		lowestRoll = min(lowestRoll, roll)
	}

	return score - lowestRoll
}

// GenerateCharacter creates a new Character with random scores for abilities
func GenerateCharacter() Character {
	constitution := Ability()

	character := Character{
		Strength:     Ability(),
		Dexterity:    Ability(),
		Constitution: constitution,
		Intelligence: Ability(),
		Wisdom:       Ability(),
		Charisma:     Ability(),
		Hitpoints:    10 + Modifier(constitution),
	}

	return character
}

// rollDie simulates rolling a d-sided die.
func rollDie(rng *rand.Rand, d int) int {
	return rng.Intn(d) + 1
}
