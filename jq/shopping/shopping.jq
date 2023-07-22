.name,
(.ingredients | length),
(.ingredients[] | select(.item == "sugar") | .amount.quantity),
(
  .ingredients + .["optional ingredients"] |
  map(select(has("substitute"))) |
  reduce .[] as $i ({}; .[$i.item] = $i.substitute)
)
