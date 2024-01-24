from std/sequtils import foldl

proc reverse*(s: string): string =
  foldl(s, b & a, "")
