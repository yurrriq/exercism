(define-module (bob)
  #:export (response-for))

(define (response-for prompt)
  (cond
   ((string-every char-whitespace? prompt)         "Fine. Be that way!")
   ((and (string-any char-alphabetic? prompt)
         (string=? prompt (string-upcase prompt))) "Whoa, chill out!")
   ((string-suffix? "?" prompt)                    "Sure.")
   (else                                           "Whatever.")))
