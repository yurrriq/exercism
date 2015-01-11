(defpackage #:dna
  (:use #:cl)
  (:export #:hamming-distance))
(in-package #:dna)

;; Overfastidiously, I find `(= (length x) (length y))` ehgo unpleasant.
;; I know mapping `#'length` and `apply`ing `'=` is overkill, especially when
;; there are only two `args`, but `length=` is much more elegant-looking to me.
(defun length= (&rest args)
  "Returns `T` iff the length of each `args` is equal, otherwise `nil`."
  (apply '= (mapcar #'length args)))

(defun hamming-distance (dna1 dna2)
  "Determine number of mutations between DNA strands by computing the Hamming Distance."
  (when (length= dna1 dna2)
    (count t (map 'list #'char/= dna1 dna2))))
