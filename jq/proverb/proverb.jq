def windows($k):
  . as $xs |
  if $xs | length | . >= $k then
    [$xs[0:$k]] + ($xs[1:] | windows($k))
  else
    []
  end
;

.strings as $strings |
$strings |
if $strings == [] then
  []
else
  $strings |
  windows(2) |
  map(. as [$x, $y] | "For want of a \($x) the \($y) was lost.") +
  ["And all for the want of a \($strings[0])."]
end
