#lang racket

(require (only-in point-free define/wind-pre wind-pre*)
         (only-in predicates and? not?)
         (only-in threading  λ~>>))

(provide anagrams-for)

(define (string-ci=?? word)
  (λ~>> (string-ci=? word)))

(define string-sort-ci
  (λ~>> string->list (sort _ char-ci<?) list->string))

(define (anagram?? word)
  (and? (not? (string-ci=?? word))
        (λ~>> ((wind-pre* string-ci=? string-sort-ci) word))))

(define/wind-pre anagrams-for
  filter anagram?? identity)
