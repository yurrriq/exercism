fun accumulate []      _ = []
  | accumulate (x::xs) f = f  :: accumulate xs f
