def known_colors:
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
  .colors as $colors |
  (
    (known_colors | index($colors[0]) * 10) +
    (known_colors | index($colors[1]))
  ) *
  (pow(10; (known_colors | index($colors[2]))))
;

def label_ohms:
  . as $value |
  if $value == 0 then
    {value: 0, unit: "ohms"}
  else
    [ [1000000000, "gigaohms"]
    , [1000000, "megaohms"]
    , [1000, "kiloohms"]
    ] |
    map(select($value % first == 0)) |
    first // [1, "ohms"] |
    {value: ($value / first), unit: last}
  end
;

value | label_ohms
