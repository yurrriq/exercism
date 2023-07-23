.number as $number |
[] |
if remainder($number; 3) == 0 then
  . += ["Pling"]
else
  .
end |
if remainder($number; 5) == 0 then
  . += ["Plang"]
else
  .
end |
if remainder($number; 7) == 0 then
  . += ["Plong"]
else
  .
end |
if . == [] then
  [$number | tostring]
else
  .
end |
join("")
