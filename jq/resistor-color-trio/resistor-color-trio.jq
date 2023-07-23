def colors:
  [ "black"
  , "brown"
  , "red"
  , "orange"
  , "yellow"
  , "green"
  , "blue"
  , "violet"
  , "grey"
  , "white"
  ]
;

def prefixes:
  [ ""
  , "kilo"
  , "mega"
  , "giga"
  ]
;

def value:
  . as $color | colors | index($color)
;

.colors | map(value) as [$tens, $ones, $zeros] |
{
  value: ((10 * $tens + $ones) * pow(10; ($zeros + 1) % 3 - 1)),
  unit: (prefixes[($zeros + 1) / 3 | floor] + "ohms")
}
