package raindrops

import (
	"strconv"
)

var (
	numbers = []int{3, 5, 7}
	words   = []string{"Pling", "Plang", "Plong"}
)

func Convert(number int) (result string) {
	for i, divisor := range numbers {
		if number%divisor == 0 {
			result += words[i]
		}
	}

	if result == "" {
		result = strconv.Itoa(number)
	}

	return result
}
