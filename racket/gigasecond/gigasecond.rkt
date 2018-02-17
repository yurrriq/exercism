#lang racket

(require (only-in racket/date date->seconds))

(provide add-gigasecond)

(define (add-gigasecond date) (seconds->date (+ (date->seconds date) 1e9)))
