#lang racket

(provide square total)

(define (square n) (expt 2 (sub1 n)))

(define (total) (sub1 (square 65)))
