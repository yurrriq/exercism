;; -*- compile-command: "sbcl --non-interactive --load *-test.lisp" -*-

(in-package #:cl-user)
(defpackage #:dna
  (:use #:cl)
  (:export #:to-rna))
(in-package #:dna)

(defconstant dna->rna
  '((#\G . #\C)
    (#\C . #\G)
    (#\T . #\A)
    (#\A . #\U)))

(defun transcribe (nucleotide)
  "Given a DNA nucleotide, returns its RNA complement,
  or signals an error when an invalid nucleotide is encountered."
  (or (cdr (assoc nucleotide dna->rna))
      (signal 'error)))

(defun to-rna (dna)
  "Given a DNA strand, returns its transcribed RNA strand."
  (concatenate 'string (mapcar #'transcribe (coerce dna 'list))))
