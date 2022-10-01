package thefarm

import (
	"errors"
	"fmt"
)

type SillyNephewError struct {
	cows int
}

func (err *SillyNephewError) Error() string {
	return fmt.Sprintf("silly nephew, there cannot be %d cows", err.cows)
}

// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(weightFodder WeightFodder, cows int) (float64, error) {
	// 1. Get the amount of fodder from the FodderAmount method
	fodder, err := weightFodder.FodderAmount()
	if err != nil {
		if err == ErrScaleMalfunction {
			if fodder > 0 {
				fodder *= 2
			}
		} else {
			return 0, err
		}
	}

	// 2. Return an error for negative fodder
	if fodder < 0 {
		return 0, errors.New("negative fodder")
	}

	// 3. Prevent division by zero
	if cows == 0 {
		return 0, errors.New("division by zero")
	}

	// 4. Handle negative cows
	if cows < 0 {
		return 0, &SillyNephewError{cows}
	}

	return fodder / float64(cows), nil
}
