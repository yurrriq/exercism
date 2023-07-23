def distance_from_origin:
  .x * .x + .y * .y | sqrt
;

distance_from_origin as $distance |
if $distance > 10 then
  0
elif $distance > 5 then
  1
elif $distance > 1 then
  5
else
  10
end
