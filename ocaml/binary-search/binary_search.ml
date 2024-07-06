let rec find haystack needle =
  let len = Array.length haystack in
  if len == 0 then Error "value not in array"
  else
    let pivot = Array.length haystack / 2 in
    let middle = haystack.(pivot) in
    if needle < middle then find (Array.sub haystack 0 pivot) needle
    else if needle > middle then
      let pos = pivot + 1 in
      Array.sub haystack pos (len - pos)
      |> (Fun.flip find) needle
      |> Result.map (( + ) (pivot + 1))
    else Ok pivot
