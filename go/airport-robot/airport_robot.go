// Package airportrobot implements the [Airport Robot] exercise from Exercism.
//
// [Airport Robot]: https://exercism.org/tracks/go/exercises/airport-robot
package airportrobot

import (
	"fmt"
)

// A Greeter can greet people in their native language.
type Greeter interface {
	LanguageName() string
	Greet(visitorName string) string
}

// SayHello greets a visitor.
func SayHello(visitorName string, greeter Greeter) string {
	return fmt.Sprintf("I can speak %s: %s",
		greeter.LanguageName(),
		greeter.Greet(visitorName),
	)
}

// Italian is a Greeter.
type Italian struct{}

// LanguageName returns Italian.
func (Italian) LanguageName() string {
	return "Italian"
}

// Greet a visitor in Italian.
func (Italian) Greet(visitorName string) string {
	return fmt.Sprintf("Ciao %s!", visitorName)
}

// Portuguese is a Greeter.
type Portuguese struct{}

// LanguageName returns Portuguese.
func (Portuguese) LanguageName() string {
	return "Portuguese"
}

// Greet a visitor in Portuguese.
func (Portuguese) Greet(visitorName string) string {
	return fmt.Sprintf("Olá %s!", visitorName)
}
