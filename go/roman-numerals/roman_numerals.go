// Package romannumerals implements the Roman Numerals exercise.
package romannumerals

import (
	"bytes"
	"fmt"
	"strings"
)

type conversion struct {
	arabic int
	roman  string
}

// ToRomanNumeral converts a given int (0 < n < 4000) to its roman numeral
// string representation.
func ToRomanNumeral(input int) (string, error) {
	if input < 1 || input > 3999 {
		return "", fmt.Errorf("%d is not within 1 and 3999, inclusive", input)
	}

	toConvert := input
	buffer := bytes.NewBufferString("")
	for _, conv := range arabicToRoman() {
		if conv.arabic <= toConvert {
			buffer.WriteString(strings.Repeat(conv.roman, toConvert/conv.arabic))
			toConvert %= conv.arabic
		}
	}

	return buffer.String(), nil
}

func arabicToRoman() []conversion {
	return []conversion{
		{1000, "M"},
		{900, "CM"},
		{500, "D"},
		{400, "CD"},
		{100, "C"},
		{90, "XC"},
		{50, "L"},
		{40, "XL"},
		{10, "X"},
		{9, "IX"},
		{5, "V"},
		{4, "IV"},
		{1, "I"},
	}
}
