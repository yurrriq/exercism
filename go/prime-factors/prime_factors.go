// Package prime implements the [Prime Factors] exercise from Exercism.
//
// [Prime Factors]: https://exercism.org/tracks/go/exercises/prime-factors
package prime

// Factors computes the primte factors of a given natural number.
func Factors(n int64) (factors []int64) {
	for i := int64(2); i <= n; i++ {
		for n%i == 0 {
			factors = append(factors, i)
			n /= i
		}
	}

	return
}
