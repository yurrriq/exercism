(defpackage #:dna
  (:use #:cl)
  (:export #:hamming-distance))
(in-package #:dna)

;; This is a rather direct port of my Clojure solution. 
(defun hamming-distance (dna1 dna2)
  "Determine number of mutations between DNA strands by computing the Hamming Distance."
  (when (apply '= (mapcar 'length (list dna1 dna2)))
    (reduce #'(lambda (n different?) (if different? (incf n) n))
            (mapcar #'(lambda (a b) (not (char= a b)))
                    (coerce dna1 'list)
                    (coerce dna2 'list))
            :initial-value 0)))
