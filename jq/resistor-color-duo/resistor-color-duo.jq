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

def value:
  . as $color | colors | index($color)
;

.colors |
map(value) as [$tens, $ones] |
10 * $tens + $ones
