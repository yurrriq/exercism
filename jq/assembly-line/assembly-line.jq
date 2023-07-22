def production_rate_per_hour:
  if (1 <= . and . <= 4) then
    . * 1.0
  elif (5 <= . and . <= 8) then
    . * 0.9
  elif (. == 9) then
    . * 0.8
  else
    . * 0.77
  end |
  . * 221
;

def working_items_per_minute:
  production_rate_per_hour / 60 | floor
;

.speed | (production_rate_per_hour, working_items_per_minute)
