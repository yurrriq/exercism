package armstrong

import (
	"math"
	"strconv"
)

func IsNumber(n int) bool {
	if n == 0 {
		return true
	}

	k := numDigits(n)

	sum := 0
	for _, digit := range strconv.Itoa(n) {
		sum += pow(int(digit-'0'), k)
	}

	return n == sum
}

func numDigits(n int) int {
	return int(math.Floor(math.Log10(float64(n)))) + 1
}

// NOTE: doesn't handle negative exponents
func pow(x, y int) int {
	var result, i int

	result = 1

	for i = 0; i < y; i++ {
		result *= x
	}

	return result
}
