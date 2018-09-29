open Base
open Base.Continue_or_stop


(** Determine whether a given character represents a nucleotide. *)
let is_nucleotide = function
  | 'A' | 'C' | 'G' | 'T' -> true
  | _                     -> false


(** Count the occurrences of a given nucleotide in a given string. *)
let count_nucleotide string char =
  let count_nucleotide_iter = fun count char' ->
    if not (is_nucleotide char') then
      Stop (Error char')
    else
      Continue (if Char.equal char char' then
                  count + 1
                else
                  count) in
  if not (is_nucleotide char) then
    Error char
  else
    String.fold_until string
      ~init:0
      ~finish:Result.return
      ~f:count_nucleotide_iter


(** Count the occurrences of each nucleotide in a given string. *)
let count_nucleotides =
  let increment =
    Map.change ~f:(function
        | None   -> Some 1
        | Some n -> Some (n + 1)) in
  let count_nucleotides_iter = fun acc char ->
    if is_nucleotide char then
      Continue (increment acc char)
    else
      Stop (Error char) in
  String.fold_until
    ~init:(Map.empty (module Char))
    ~finish:Result.return
    ~f:count_nucleotides_iter
