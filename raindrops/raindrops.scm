(define-module (raindrops)
  #:export (convert))

(define (divides? d n)
  "Returns #t if d divides n, otherwise #f."
  (zero? (modulo n d)))

(define (convert n)
  (define (number->raindrops n)
    "Given a number, returns a possibly empty list of raindrops."
    (filter string? (map (λ (x) (and (divides? (car x) n) (cadr x)))
                         '((3 "Pling") (5 "Plang") (7 "Plong")))))
  (let ((raindrops (number->raindrops n)))
    (if (null? raindrops)
        (number->string n)
        (string-concatenate raindrops))))


;; Local Variables:
;; compile-command: "guile raindrops-test.scm
;; End:
