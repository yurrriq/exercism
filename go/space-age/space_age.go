package space

import (
	"errors"
)

type Planet string

const (
	Mercury Planet = "Mercury"
	Venus          = "Venus"
	Earth          = "Earth"
	Mars           = "Mars"
	Jupiter        = "Jupiter"
	Saturn         = "Saturn"
	Uranus         = "Uranus"
	Neptune        = "Neptune"
)

const EarthOrbitalPeriod float64 = 365.25 * 24 * 60 * 60

func Age(seconds float64, planet Planet) float64 {
	period, err := orbitalPeriodInEarthYears(planet)
	if err != nil {
		return period
	}

	return seconds / (period * EarthOrbitalPeriod)
}

func orbitalPeriodInEarthYears(planet Planet) (float64, error) {
	switch planet {
	case Mercury:
		return 0.2408467, nil
	case Venus:
		return 0.61519726, nil
	case Earth:
		return 1, nil
	case Mars:
		return 1.8808158, nil
	case Jupiter:
		return 11.862615, nil
	case Saturn:
		return 29.447498, nil
	case Uranus:
		return 84.016846, nil
	case Neptune:
		return 164.79132, nil
	default:
		return -1, errors.New("Unknown planet")
	}
}
