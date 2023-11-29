open Base

type nucleotide =
  | A (** Adenine *)
  | C (** Cytosine *)
  | G (** Guanine *)
  | T (** Thymine *)

(** [hamming_distance xs ys] computes the Hamming distance between the two lists. *)
val hamming_distance : nucleotide list -> nucleotide list -> (int, string) Result.t
