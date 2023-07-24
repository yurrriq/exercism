# input: a floating point number
# output: the number rounded to two decimal places
def two_decimal: ((. * 100) | round) / 100;

def seconds_in_year:
  365.25 * 24 * 60 * 60
;

def planets:
  {
    Earth: 1.0,
    Mercury: 0.2408467,
    Venus: 0.61519726,
    Mars: 1.8808158,
    Jupiter: 11.862615,
    Saturn: 29.447498,
    Uranus: 84.016846,
    Neptune: 164.79132
  }
;

.seconds / seconds_in_year / (planets[.planet] // ("not a planet" | halt_error)) |
two_decimal
