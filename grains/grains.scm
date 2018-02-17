;; -*- compile-command: "guile grains-test.scm" -*-

(define-module (grains)
  #:export (square total)
  #:autoload (srfi srfi-1) (iota))

(define (square n)
  (expt 2 (1- n)))

(define (total)
  (1- (square 65)))
