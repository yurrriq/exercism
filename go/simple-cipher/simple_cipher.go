// Package cipher implements the Simple Cipher exercise from Exercism.
package cipher

import (
	"strings"
	"unicode"
)

type shift struct {
	distance int
}

type vigenere struct {
	key string
}

// NewCaesar creates a new Caesar cipher.
func NewCaesar() Cipher {
	return NewShift(3)
}

// NewShift creates a new shift cipher with a given shift distance.
func NewShift(distance int) Cipher {
	absDistance := abs(distance)
	if absDistance <= 0 || absDistance >= 26 {
		return nil
	}

	return shift{distance}
}

func (c shift) Encode(message string) string {
	var builder strings.Builder
	builder.Grow(len(message))
	for _, roon := range message {
		if !unicode.IsLetter(roon) {
			continue
		}
		builder.WriteRune(shiftRune(c.distance, unicode.ToLower(roon)))
	}

	return builder.String()
}

func (c shift) Decode(message string) string {
	decoder := shift{-c.distance}
	return decoder.Encode(message)
}

// NewVigenere creates a new Vigenère cipher.
func NewVigenere(key string) Cipher {
	if key == "" {
		return nil
	}

	asOnly := true
	for _, roon := range key {
		if !unicode.IsLetter(roon) || !unicode.IsLower(roon) {
			return nil
		}
		asOnly = asOnly && roon == 'a'
	}

	if asOnly {
		return nil
	}

	return vigenere{key}
}

func (v vigenere) Encode(message string) string {
	return cipher(id, v.key, message)
}

func (v vigenere) Decode(message string) string {
	return cipher(negate, v.key, message)
}

func cipher(f func(int) int, key string, message string) string {
	if message == "" || key == "" {
		return ""
	}

	keyRunes := []rune(key)
	keyLength := len(keyRunes)

	ciphered := []rune{}
	var i int
	for _, roon := range message {
		if !unicode.IsLetter(roon) {
			continue
		}
		distance := f(letterToInt(keyRunes[i%keyLength]))
		ciphered = append(ciphered, shiftRune(distance, unicode.ToLower(roon)))
		i++
	}

	return string(ciphered)
}

func shiftRune(distance int, roon rune) rune {
	return 'a' + rune(mod(letterToInt(roon)+distance, 26))
}

func letterToInt(letter rune) int {
	return int(letter - 'a')
}

func abs(n int) int {
	if n < 0 {
		n = -n
	}
	return n
}

func mod(a, n int) int {
	return (a%n + n) % n
}

func negate(i int) int {
	return -i
}

func id[T any](x T) T {
	return x
}
