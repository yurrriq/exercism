(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands."
  (when (funcall (on #'= #'length) dna1 dna2)
    (count 'nil (map 'list #'char= dna1 dna2))))

;; Based on http://stackoverflow.com/a/31755514/1793234
(defun on (f g)
  (lambda (x y)
    (funcall f
             (funcall g x)
             (funcall g y))))
