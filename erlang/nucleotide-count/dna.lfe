(defmodule dna
  (export (count 2)
          (nucleotide_counts 1)))

(defun validate (nucleotide)
  (if (lists:member nucleotide '("A" "C" "G" "T"))
    `#(ok ,nucleotide)
    (erlang:error "Invalid nucleotide")))

(defun count (strand nucleotide)
  ;; `validate` will throw iff `nucleotide` is invalid.
  (validate nucleotide)
  (lists:foldl
    (lambda (nucleotide* sum)
      (if (=:= (validate `(,nucleotide*)) `#(ok ,nucleotide))
        (+ sum 1)
        sum))
    0
    strand))

(defun nucleotide_counts (strand)
  (lists:map
    (lambda (nucleotide)
      `#(,nucleotide ,(count strand nucleotide)))
    `("A" "T" "C" "G")))


;; Local Variables:
;; compile-command: "lfec dna.lfe; and erlc dna_tests.erl; and erl -noshell -eval 'eunit:test(dna_tests, [verbose])' -s init stop"
;; End:
