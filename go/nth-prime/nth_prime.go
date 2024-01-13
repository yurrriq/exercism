package prime

import (
	"errors"
)

// Nth returns the nth prime number. An error must be returned if the nth prime
// number can't be calculated.
func Nth(n int) (p int, err error) {
	if n < 0 {
		err = errors.New("n must be positive")
	}

	if n == 0 {
		err = errors.New("there is no zeroth prime")
	}

	if err != nil {
		return p, err
	}

	ch := make(chan int)
	go generate(ch)
	var prime int
	for i := 0; i < n; i++ {
		prime = <-ch
		ch1 := make(chan int)
		go filter(ch, ch1, prime)
		ch = ch1
	}

	return prime, nil
}

func generate(ch chan<- int) {
	for i := 2; ; i++ {
		ch <- i
	}
}

func filter(in <-chan int, out chan<- int, prime int) {
	for {
		i := <-in
		if i%prime != 0 {
			out <- i
		}
	}
}
