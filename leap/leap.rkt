#lang racket

(require (only-in math/number-theory divides?))

(provide leap-year?)

(define (leap-year? year)
  (and (divides? 4 year)
       (or (divides? 400 year)
           (not (divides? 100 year)))))
