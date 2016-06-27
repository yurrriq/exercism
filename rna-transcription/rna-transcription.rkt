#lang racket

(require (only-in lens          assoc-lens lens-view)
         ;; (only-in unstable/lens lens-view~>)
         (only-in threading     λ~> ~>>))

(provide to-rna)

(define (to-rna dna)
  (~>> (string->list dna)
       (map (λ~> assoc-lens (lens-view dna->rna)))
       (apply string)))

(define dna->rna
  '((#\C . #\G)
    (#\G . #\C)
    (#\A . #\U)
    (#\T . #\A)))
