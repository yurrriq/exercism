package collatzconjecture

const ErrInvalidNumber CollatzError = "Only strictly positive numbers are allowed"

type CollatzError string

func (err CollatzError) Error() string {
	return string(err)
}

func CollatzConjecture(n int) (int, error) {
	if n <= 0 {
		return -1, ErrInvalidNumber
	}

	var steps int
	for steps = 0; n != 1; steps++ {
		if 0 == n&1 {
			n /= 2
		} else {
			n *= 3
			n++
		}
	}

	return steps, nil
}
