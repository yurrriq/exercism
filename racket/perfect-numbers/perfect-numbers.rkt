#lang racket

(require (only-in math/base sum)
         (only-in math/number-theory divides?))

(provide perfect-numbers)

(define (perfect-numbers n) (filter perfect? (range 1 (add1 n))))

(define (perfect? n)
  (let ([divisor? (lambda (x) (divides? x n))])
    (= n (sum (filter divisor? (range 1 n))))))
