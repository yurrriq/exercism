#lang racket

(require (only-in srfi/13
                  string-any
                  string-every))

(provide response-for)

(define (response-for prompt)
  (cond
    [(string-every char-whitespace? prompt)         "Fine. Be that way!"]
    [(and (string-any char-upper-case? prompt)
          (string=? prompt (string-upcase prompt))) "Whoa, chill out!"]
    [(string-suffix? prompt "?")                    "Sure."]
    (else                                           "Whatever.")))
