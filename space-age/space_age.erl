-module(space_age).
-export([ageOn/2]).

ageOn(Planet, Seconds) -> Seconds / secondsInYear(Planet).

secondsInYear(earth)   -> 365.25 * 24 * 60 * 60;
secondsInYear(mercury) -> secondsInYear(0.2408467);
secondsInYear(venus)   -> secondsInYear(0.61519726);
secondsInYear(mars)    -> secondsInYear(1.8808158);
secondsInYear(jupiter) -> secondsInYear(11.862615);
secondsInYear(saturn)  -> secondsInYear(29.447498);
secondsInYear(uranus)  -> secondsInYear(84.016846);
secondsInYear(neptune) -> secondsInYear(164.79132);
secondsInYear(Seconds) -> Seconds * secondsInYear(earth).
