#lang racket

(provide hello)

(define hello
  (lambda ([name "World"])
    (format "Hello, ~a!" name)))
