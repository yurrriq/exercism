def encode_iter($c; $cs):
  if $cs == "" then
    $c
  else
    "\($cs | length + 1)" + $c
  end
;

def encode:
  reduce scan("(.)(\\1*)") as [$c, $cs] (""; . + encode_iter($c; $cs))
;

def decode_iter($n; $c):
  if $n == "" then
    $c
  else
    $c * ($n | tonumber)
  end
;

def decode:
  reduce scan("(\\d*)(\\D)") as [$n, $c] (""; . + decode_iter($n; $c))
;
