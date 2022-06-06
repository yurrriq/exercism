// Package weather forecast the current weather condition of various cities.
package weather

// CurrentCondition is the current weather condition.
var CurrentCondition string

// CurrentLocation is the current location.
var CurrentLocation string

// Forecast the weather for a given city and condition.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
