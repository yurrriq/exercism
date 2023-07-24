(.limit + 1) as $limit |
reduce range(2; $limit) as $n ([range(2; $limit)];
  if contains([$n]) then
    reduce range($n + $n; $limit; $n) as $composite (.; . - [$composite])
  else
    .
  end
)
