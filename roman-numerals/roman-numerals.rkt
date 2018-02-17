#lang racket

(require (only-in racket/match  match-lambda**)
         (only-in racket/string string-append*))

(provide to-roman)

(define (to-roman x) (to-roman* x arabic->roman ""))

(define to-roman*
  (match-lambda**
   [(x '() roman) roman]
   [(x `((,a ,r) . ,rst) roman)
    #:when (>= x a)
    (let*-values ([(n x*)   (quotient/remainder x a)]
                  [(roman*) (string-append roman (replicate n r))])
      (to-roman* x* rst roman*))]
   [(x `(,_ . ,rst) roman)
    (to-roman* x rst roman)]))

(define arabic->roman
  '[(1000 "M") (900 "CM") (500 "D") (400 "CD")
    (100  "C") (90  "XC") (50  "L") (40  "XL")
    (10   "X") (9   "IX") (5   "V") (4   "IV")
    (1    "I")])

(define (replicate n str) (string-append* (make-list n str)))
