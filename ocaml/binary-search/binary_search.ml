let find haystack needle =
  let rec find_between left right =
    if left > right then Error "value not in array"
    else
      let pivot = (left + right) / 2 in
      let middle = haystack.(pivot) in
      if needle < middle then find_between left (pivot - 1)
      else if needle > middle then find_between (pivot + 1) right
      else Ok pivot
  in
  find_between 0 (Array.length haystack - 1)
