(define-module (leap-year)
  #:export (leap-year?))

(define (leap-year? year)
  (and (zero? (modulo year 4))
       (or (zero? (modulo year 400))
           (positive? (modulo year 100)))))
