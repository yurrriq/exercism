(define-module (leap-year)
  #:export (leap-year?))

(define (leap-year? year)
  (and (zero? (modulo year 4))
       (or (positive? (modulo year 100))
           (zero? (modulo year 400)))))
