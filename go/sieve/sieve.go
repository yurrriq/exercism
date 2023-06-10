package sieve

import (
	"math"
)

func Sieve(limit int) []int {
	if limit < 2 {
		return nil
	}

	flimit := float64(limit)

	sieve := make([]bool, limit+1)
	for i := 2; i <= int(math.Sqrt(flimit)); i++ {
		if !sieve[i] {
			for j := i * i; j <= limit; j += i {
				sieve[j] = true
			}
		}
	}

	primes := make([]int, 0, int(flimit/math.Log(flimit)))
	for i := 2; i <= limit; i++ {
		if !sieve[i] {
			primes = append(primes, i)
		}
	}

	return primes
}
