(define-module (grains)
  #:export (square total)
  #:autoload (srfi srfi-1) (iota))

(define (square n)
  (expt 2 (1- n)))

;; Traverses the list only once.
(define (total)
  (reduce (lambda (n acc) (+ acc (square n))) 0 (iota 64 1)))
