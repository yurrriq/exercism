package sieve

import (
	"math"
	"sort"
)

func Sieve(limit int) []int {
	if limit <= 1 {
		return nil
	}

	sieve := make(map[int]struct{}, limit-1)
	for i := 2; i <= limit; i++ {
		sieve[i] = struct{}{}
	}

	sqrtLimit := int(math.Ceil(math.Sqrt(float64(limit))))
	for i := 2; i <= sqrtLimit; i++ {
		if _, present := sieve[i]; present {
			for j := i * i; j <= limit; j += i {
				delete(sieve, j)
			}
		}
	}

	keys := make([]int, 0, len(sieve))
	for k := range sieve {
		keys = append(keys, k)
	}

	sort.SliceStable(keys, func(i, j int) bool {
		return keys[i] < keys[j]
	})

	return keys
}
