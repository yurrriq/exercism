#lang racket

(require (only-in srfi/1 reduce iota))

(provide sum-of-squares square-of-sums difference)

(define (square n) (expt n 2))

(define (square-of-sums n) (truncate (square (/ (* n (add1 n)) 2))))

(define (sum-of-squares n) (truncate (/ (* n (add1 n) (add1 (* 2 n))) 6)))

(define (difference n) (- (square-of-sums n) (sum-of-squares n)))
