// Package prime implements the [Prime Factors] exercise from Exercism.
//
// [Prime Factors]: https://exercism.org/tracks/go/exercises/prime-factors
package prime

// Factors computes the primte factors of a given natural number.
func Factors(n int64) (factors []int64) {
	var p int64 = 2
	for {
		if n%p == 0 {
			factors = append(factors, p)
			n /= p
		} else if n == 1 {
			break
		} else {
			p++
		}
	}

	return factors
}
