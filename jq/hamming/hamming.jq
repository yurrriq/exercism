[.strand1, .strand2] | map(explode) as [$strand1, $strand2] |
if ($strand1 | length) != ($strand2 | length) then
  "strands must be of equal length" | halt_error
else
  [range($strand1 | length)] |
  map(select($strand1[.] != $strand2[.])) |
  length
end
