pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  foldr(first, second, fn(acc, x) { [x, ..acc] })
}

pub fn concat(lists: List(List(a))) -> List(a) {
  foldl(lists, [], append)
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  foldr(list, [], fn(ys, x) {
    case function(x) {
      True -> [x, ..ys]
      False -> ys
    }
  })
}

pub fn length(list: List(a)) -> Int {
  foldl(list, 0, fn(acc, _) { acc + 1 })
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  foldr(list, [], fn(acc, x) { [function(x), ..acc] })
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [x, ..xs] -> foldl(xs, function(initial, x), function)
  }
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [x, ..xs] -> function(foldr(xs, initial, function), x)
  }
}

pub fn reverse(list: List(a)) -> List(a) {
  foldl(list, [], fn(acc, x) { [x, ..acc] })
}
