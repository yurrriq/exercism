package airportrobot

import "fmt"

type Greeter interface {
	LanguageName() string
	Greet(visitorName string) string
}

func SayHello(visitorName string, greeter Greeter) string {
	return fmt.Sprintf("I can speak %s: %s",
		greeter.LanguageName(),
		greeter.Greet(visitorName),
	)
}

type Italian struct{}

func (_ Italian) LanguageName() string {
	return "Italian"
}

func (_ Italian) Greet(visitorName string) string {
	return fmt.Sprintf("Ciao %s!", visitorName)
}

type Portuguese struct{}

func (_ Portuguese) LanguageName() string {
	return "Portuguese"
}

func (_ Portuguese) Greet(visitorName string) string {
	return fmt.Sprintf("Olá %s!", visitorName)
}
