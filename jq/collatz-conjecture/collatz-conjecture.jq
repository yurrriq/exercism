def is_even:
  remainder(.; 2) == 0
;

def collatz:
  if is_even then
    . / 2
  else
    3 * . + 1
  end
;

def steps:
  if 0 < . then
    [while(. != 1; collatz)] | length
  else
    "Only positive integers are allowed" | halt_error
  end
;
