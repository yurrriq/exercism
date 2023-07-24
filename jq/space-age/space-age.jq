# input: a floating point number
# output: the number rounded to two decimal places
def two_decimal: ((. * 100) | round) / 100;

def seconds_in_year($planet):
  if $planet == "Earth" then
    365.25 * 24 * 60 * 60
  elif $planet == "Mercury" then
    0.2408467 * seconds_in_year("Earth")
  elif $planet == "Venus" then
    0.61519726 * seconds_in_year("Earth")
  elif $planet == "Mars" then
    1.8808158 * seconds_in_year("Earth")
  elif $planet == "Jupiter" then
    11.862615 * seconds_in_year("Earth")
  elif $planet == "Saturn" then
    29.447498 * seconds_in_year("Earth")
  elif $planet == "Uranus" then
    84.016846 * seconds_in_year("Earth")
  elif $planet == "Neptune" then
    164.79132 * seconds_in_year("Earth")
  else
    "not a planet" | halt_error
  end
;

.seconds / seconds_in_year(.planet) |
two_decimal
