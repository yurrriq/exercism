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

if .property == "colors" then
  colors
elif .property == "colorCode" then
  .input.color | value
else
  "Unknown property: \(.property)\n" | halt_error
end
