def pling:
  if remainder(.; 3) == 0 then
    "Pling"
  else
    ""
  end
;

def plang:
  if remainder(.; 5) == 0 then
    "Plang"
  else
    ""
  end
;

def plong:
  if remainder(.; 7) == 0 then
    "Plong"
  else
    ""
  end
;

.number | "\(pling)\(plang)\(plong)" as $sound |
if $sound == "" then
  .
else
  $sound
end
