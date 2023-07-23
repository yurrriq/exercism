def sum:
  reduce .[] as $item (0; . + $item)
;

{
  last_week: (.[-2]),
  yesterday: (last | .[-2]),
  total: (last | sum),
  busy_days: (last | map(select(. >= 5)) | length),
  has_day_without_birds: (last | map(select(. == 0)) | . != [])
}
